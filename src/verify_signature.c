#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <openssl/dsa.h>
#include <openssl/evp.h>
#include <openssl/pem.h>
#include <openssl/err.h>

static char *datafile = 0;

#define SMALL_BUFFER_SIZE 256
#define BIG_BUFFER_SIZE 1024

void fix_crlf_problem(char * data) {
	char cr  = 0x0d;
	char lf = 0x0a; 
	while (*data) {
		if (*data == cr) {
			*data     = lf;
			*(data+1) = 0;
			return;
		}
		data++;
	}
}

// Reads the message and signature from the file pointed to by
// the SRI_LICENSE_CERTIFICATE environment variable.
// Puts the message into msg, the signature into sig, and
//      returns the length of sig. 
int read_message_and_signature (char* sysname, char* msg, unsigned char* sig) {
	FILE *fp;
	int fd;
	char envstr[SMALL_BUFFER_SIZE];
	char sysuppercase[SMALL_BUFFER_SIZE];
	char syslowercase[SMALL_BUFFER_SIZE];
	char beginstr[SMALL_BUFFER_SIZE];
	char endstr[SMALL_BUFFER_SIZE];
	char data[SMALL_BUFFER_SIZE];
	int in_msg = 0;
	BIO *in, *b64;
	int cnt = 0;
	int size;
	long sigstart = 0;
	int i;
	
	for (i = 0; i < strlen(sysname); i++)
		 sysuppercase[i] = toupper(sysname[i]);
	sysuppercase[i] = 0;
	
	for (i = 0; i < strlen(sysname); i++)
		syslowercase[i] = tolower(sysname[i]);
	syslowercase[i] = 0;
	
	// Make sure msg starts life as an empty string, for strcat to work correctly
	msg[0] = 0;
	beginstr[0] = 0;
	strcat(beginstr, "# Beginning of signed ");
	strcat(beginstr, sysuppercase);
	strcat(beginstr, " message\n");
	endstr[0] = 0;
	strcat(endstr, "# End of signed ");
	strcat(endstr, sysuppercase);
	strcat(endstr, " message\n");
	
	// Get the file path from the environment variable
	
	envstr[0] = 0;
	strcat(envstr, sysuppercase);
	strcat(envstr, "_LICENSE_CERTIFICATE");
	datafile = getenv(envstr);
	if (!datafile) {
		fprintf(stderr,"%s environment variable must be set to a\
 file you should have received in email after registering at\
 %s.csl.sri.com\n",
			 envstr, syslowercase);
		exit(1);
	}
	
	
	// Try and open it using open, so we can use read to find out if it
	// is a directory (is there a better way?)
	 if ((fd = open(datafile, O_RDONLY))==-1) {
		 fprintf(stderr, "There was a problem opening the file pointed to by the\n\
 %s environment variable:\n  %s: %s\nThis environment variable must be \
 set to a file you should have\nreceived in email after registering at \
 %s.csl.sri.com\n", envstr, datafile, strerror(errno), syslowercase);
		 exit(1);
	 }
	 
	 // Now we read from it, to check if it is a directory
	 if ((size = read(fd, data, 1)) == -1) {
		 fprintf(stderr, "There was a problem reading the file pointed to by the\n\
 %s environment variable:\n  %s: %s\nThis environment variable must be \
 set to a file you should have\nreceived in email after registering at \
 %s.csl.sri.com\n", envstr, datafile, strerror(errno), syslowercase);
		 exit(1);
	 }
	 else {
		 // read was OK, close the file
		 close(fd);
	 }
	 
	 // Now fopen the file, so we can use fgets to read lines rather than
	 // some number of chars.
	 if ((fp = fopen(datafile, "rt"))==NULL) {
		 fprintf(stderr, "There was a problem opening the file pointed to by the\n\
 %s environment variable:\n  %s: %s\nThis environment variable must be \
 set to a file you should have\nreceived in email after registering at \
 %s.csl.sri.com\n", envstr, datafile, strerror(errno), syslowercase);
		 exit(1);
	 }

	 // Loop looking for start of message, start of signature, end of signature
	 // Collect message, and note start and running length of signature.
	 do {
		 fgets(data, SMALL_BUFFER_SIZE, fp);
		 fix_crlf_problem(data);
		 if (strcmp(data, beginstr)==0) {
			 in_msg = 1;
		 }
		 else if (in_msg && (strcmp(data, "#\n")==0)) {
			 if (in_msg) {
				 sigstart = ftell(fp);
			 }
			 else {
				 fprintf(stderr, "Certificate file %s corrupted\n", datafile);
				 exit(1);
			 }
		 }
		 else if (strcmp(data, endstr)==0) {
			 break;
		 }
		 else if (sigstart)
			 cnt = ftell(fp) - sigstart;
		 else if (in_msg)
			 strcat(msg, data);
	 } while(!feof(fp));
	 
	 // Close file
	 fclose(fp);
	 
	 // Check that we found message and signature
	 if (!(in_msg && sigstart && cnt)) {
		 fprintf(stderr, "Certificate file %s corrupted\n", datafile);
		 exit(1);
	 }
	 
	 // Now try to decode base64 form of signature
	 fp = fopen(datafile, "r");
	 fseek(fp, sigstart, SEEK_SET);
	 b64 = BIO_new(BIO_f_base64());
	 in = BIO_new_fp(fp, BIO_NOCLOSE);
	 in = BIO_push(b64, in);
	 // Get to start of signature
	 //BIO_seek(in, sigstart);
	 size = BIO_read(in, sig, cnt);
	 return size;
}

int verify_signature (char* sysname) {
	unsigned char pub_key[] =
		{
			// Obtained using
			// openssl dsa -in testkey -pubout -outform DER | xxd -i
			0x30, 0x82, 0x01, 0xb7, 0x30, 0x82, 0x01, 0x2c, 0x06, 0x07, 0x2a, 0x86,
			0x48, 0xce, 0x38, 0x04, 0x01, 0x30, 0x82, 0x01, 0x1f, 0x02, 0x81, 0x81,
			0x00, 0xd1, 0x69, 0x90, 0xb3, 0x08, 0x30, 0xea, 0x9d, 0xb3, 0xd1, 0xda,
			0xa5, 0x0e, 0x97, 0x67, 0x9a, 0xc9, 0x1f, 0xa5, 0x69, 0x36, 0x5b, 0x59,
			0x11, 0x43, 0x9f, 0x3a, 0x57, 0x58, 0x21, 0x74, 0x2e, 0x2a, 0x5a, 0xda,
			0xf5, 0xb2, 0x53, 0x93, 0x5d, 0x0f, 0xd5, 0x6c, 0x8f, 0x10, 0xe8, 0x01,
			0x28, 0x63, 0x93, 0x69, 0x88, 0x2d, 0xc5, 0x9a, 0xad, 0x2f, 0x5b, 0x33,
			0xcc, 0x81, 0x9c, 0x18, 0x3b, 0xd6, 0x84, 0x06, 0xee, 0xfb, 0xf4, 0x39,
			0xce, 0xaa, 0xb1, 0x5a, 0xaf, 0x45, 0x96, 0xcd, 0x86, 0x00, 0xce, 0x76,
			0x40, 0xe8, 0xe3, 0x2d, 0xd8, 0x2b, 0x10, 0x4f, 0xad, 0xa1, 0x56, 0x92,
			0x95, 0x33, 0x1b, 0x1d, 0x93, 0x8b, 0xc0, 0x61, 0x8d, 0xae, 0x19, 0xbf,
			0x70, 0x13, 0xd0, 0x0f, 0x5a, 0xf9, 0x9b, 0x2a, 0xc4, 0x30, 0xb5, 0xc7,
			0x6e, 0x15, 0xb9, 0xc7, 0xe8, 0x06, 0xe3, 0x92, 0xaf, 0x02, 0x15, 0x00,
			0xf6, 0x56, 0xa4, 0x19, 0x1a, 0xee, 0x0b, 0x3b, 0x66, 0x55, 0x41, 0xb7,
			0xbe, 0xce, 0x89, 0x97, 0xb0, 0x7e, 0x98, 0x15, 0x02, 0x81, 0x81, 0x00,
			0xcc, 0xb2, 0xa7, 0x29, 0x05, 0xfc, 0xab, 0x49, 0x7d, 0x19, 0x24, 0x52,
			0x37, 0xda, 0x22, 0xa0, 0x33, 0xb7, 0x5c, 0x27, 0x5a, 0x6c, 0x76, 0x21,
			0x88, 0x9c, 0x1c, 0x9d, 0xe7, 0xa7, 0xcb, 0x59, 0x8f, 0x2f, 0xd4, 0x7a,
			0x81, 0xb5, 0xcb, 0x9b, 0xfc, 0xdf, 0x47, 0xea, 0xa7, 0xb1, 0x82, 0x51,
			0x1e, 0xe1, 0x6b, 0xfa, 0x4d, 0x32, 0xb2, 0x7f, 0x05, 0xc9, 0x90, 0xf8,
			0x1f, 0x19, 0xf9, 0x32, 0xe0, 0xaa, 0x41, 0x22, 0x6d, 0xe7, 0x3c, 0x56,
			0x85, 0x6b, 0x6d, 0xd6, 0x04, 0x06, 0x84, 0x5d, 0x5e, 0xc8, 0xca, 0x1f,
			0x31, 0x74, 0x5a, 0x11, 0xb5, 0x0f, 0x56, 0x96, 0x5a, 0xd5, 0xce, 0xa6,
			0xb2, 0x59, 0xd0, 0x70, 0x26, 0x96, 0x47, 0xf1, 0xb4, 0x2e, 0xfe, 0x02,
			0x37, 0xa6, 0x99, 0xee, 0x30, 0xc4, 0xe6, 0xb9, 0xe6, 0x10, 0xf9, 0x9e,
			0xa8, 0x95, 0xb6, 0xaa, 0xee, 0xad, 0x76, 0x2b, 0x03, 0x81, 0x84, 0x00,
			0x02, 0x81, 0x80, 0x50, 0x44, 0xb3, 0xd0, 0x85, 0x8f, 0xc8, 0x72, 0xb5,
			0x92, 0x31, 0xc9, 0xca, 0xb3, 0xeb, 0xf6, 0x49, 0xa1, 0xb2, 0x2a, 0x3d,
			0xcd, 0x66, 0xac, 0x5f, 0x9a, 0xaf, 0x65, 0x82, 0x7f, 0x27, 0x94, 0x09,
			0x0b, 0x81, 0xe5, 0x24, 0x50, 0x58, 0x51, 0xb9, 0x8c, 0x84, 0xfd, 0x93,
			0xbc, 0x9e, 0x62, 0x9f, 0x7e, 0xd7, 0xba, 0x0c, 0x3d, 0x21, 0xec, 0xf2,
			0xcd, 0x02, 0x0c, 0x68, 0xd7, 0x3b, 0x94, 0xdf, 0xc8, 0x5e, 0x6f, 0x2a,
			0x92, 0x60, 0xb4, 0x65, 0xad, 0x04, 0x48, 0x16, 0x4a, 0xa4, 0x24, 0xd6,
			0xe4, 0x61, 0x62, 0xae, 0x42, 0x4a, 0x5f, 0x22, 0x42, 0x69, 0x5e, 0x11,
			0x9c, 0x5c, 0x86, 0xf5, 0x30, 0x78, 0x31, 0x53, 0xe5, 0x3e, 0x85, 0x3f,
			0x42, 0xc8, 0xfa, 0xd0, 0x44, 0x42, 0x9a, 0xab, 0x2f, 0xef, 0x20, 0xc6,
			0xae, 0xa1, 0xde, 0x41, 0xf3, 0x37, 0x38, 0x6a, 0x27, 0xe6, 0x55
		};
	unsigned char *d;
	DSA *dsa;
	EVP_PKEY *pkey;
	// msg is the message part, and sig is the signature of the signed file
	char msg[BIG_BUFFER_SIZE];
	unsigned char sig[BIG_BUFFER_SIZE];
	EVP_MD_CTX ctx;
	int verified;
	unsigned int cnt = 0;
	
	static int dsa_verified = 0;
	
	if (dsa_verified) return 1;
	
	// Get the message and signature
	cnt = read_message_and_signature(sysname, msg, sig);
	
	// Convert public key text to internal form
	// bio = BIO_new(BIO_s_mem());
	
	//  bio = BIO_new_mem_buf(public_key_text, -1);
	
	// key=d2i_DSA_PUBKEY_bio(bio, NULL);
	d = pub_key;
	dsa = d2i_DSA_PUBKEY(NULL, &d, sizeof pub_key);
	/*   if ((key = PEM_read_bio_DSA_PUBKEY(bio, NULL, 0, NULL))==NULL) { */
	/*     fprintf(stderr, "Problem in PEM_read_bio_DSA_PUBKEY:\n"); */
	/*     ERR_print_errors_fp(stderr); */
	/*     exit(1); */
	/*   } */
	
	pkey = EVP_PKEY_new();
	if (!(EVP_PKEY_assign_DSA(pkey, dsa))) {
		fprintf(stderr, "Error in assign_DSA\n");
		exit(1);
	}
	
	EVP_MD_CTX_init(&ctx);
	
	if (!(EVP_VerifyInit_ex(&ctx, EVP_dss1(), NULL))) {
		fprintf(stderr, "Problem in EVP_VerifyInit_ex:\n");
		ERR_print_errors_fp(stderr);
	}
	
	if (!(EVP_VerifyUpdate(&ctx, msg, strlen(msg)))) {
		fprintf(stderr, "Problem in EVP_VerifyUpdate:\n");
		ERR_print_errors_fp(stderr);
	}
	
	verified = EVP_VerifyFinal(&ctx, sig, cnt, pkey);
	
	if (verified == 1) {
		fprintf(stderr, "%s\n", msg);
	}
	else if (verified == 0) {
		fprintf(stderr, "The license certificate %s was not verified\n",
						datafile);
		 exit(1);
	}
	else {
		fprintf(stderr, "Error in EVP_VerifyFinal\n");
		ERR_print_errors_fp(stderr);
		exit(1);
	}
	dsa_verified = 1;
	return 1;
}
