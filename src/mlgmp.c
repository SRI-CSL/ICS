/******************************************************
 * GNU MP interface for Objective CAML
 * Code by David Monniaux (David.Monniaux@ens-lyon.fr)
 * Adapted to ocaml 3.00 by Jean-Christophe Filliâtre
 ******************************************************/

#include <gmp.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <stdio.h>

/******************************* MPZ *******************************/
#define MPZ_VAL(x) (*((mpz_t*)(Data_custom_val(x))))

static void ml_mpz_finalize(value);
static int ml_mpz_compare(value, value);
static long ml_mpz_hash(value);

static struct custom_operations mpz_t_custom_operations = {
  "GMP/3.1/z",
  ml_mpz_finalize,
  ml_mpz_compare,
  ml_mpz_hash,
  custom_serialize_default,
  custom_deserialize_default
};

void ml_mpz_finalize(value v) {
  mpz_clear(MPZ_VAL(v));
}

int ml_mpz_compare(value v1, value v2) {
  return mpz_cmp(MPZ_VAL(v1), MPZ_VAL(v2));
}

long ml_mpz_hash(value v) {
  return mpz_get_ui(MPZ_VAL(v));
}

/* Alloc */
#define ALLOC_MPZ(v) \
  value v = alloc_custom(&mpz_t_custom_operations,sizeof(mpz_t),0,1)

/* Initialization functions */
value ml_mpz_from_int(value v)
{
  ALLOC_MPZ(ret);
  mpz_init_set_si(MPZ_VAL(ret), Long_val(v));
  return (value) ret;
}

value ml_mpz2_from_int(value r, value v)
{
  mpz_set_si(MPZ_VAL(r), Long_val(v));
  return r;
}

value ml_mpz_from_string(value str, value base)
{
  ALLOC_MPZ(ret);
  if (mpz_init_set_str(MPZ_VAL(ret), String_val(str), Int_val(base)))
    invalid_argument("Gmp.Z.from_string");
  return (value) ret;
}

value ml_mpz2_from_string(value r, value str, value base)
{
  if (mpz_set_str(MPZ_VAL(r), String_val(str), Int_val(base)))
    invalid_argument("Gmp.Z2.from_string");
  return r;
}

value ml_mpz_from_float(value f)
{
  ALLOC_MPZ(ret);
  mpz_init_set_d(MPZ_VAL(ret), Double_val(f));
  return (value) ret;
}

value ml_mpz2_from_float(value r, value f)
{
  mpz_set_d(MPZ_VAL(r), Double_val(f));
  return r;
}

/* Conversion to other types*/
value ml_int_from_mpz(value v)
{
  return Val_long(mpz_get_si(MPZ_VAL(v)));
}

value ml_float_from_mpz(value v)
{
  value ret=alloc(1, Double_tag);
  Store_double_val(ret, mpz_get_d(MPZ_VAL(v)));
  return ret;
}

value ml_string_from_mpz(value v, value base)
{
  char *s = mpz_get_str(NULL, Int_val(base), MPZ_VAL(v));
  value ret = copy_string(s);
  free(s);
  return ret;
}

/* Alloc and init */
value alloc_init_mpz()
{
  ALLOC_MPZ(r);
  mpz_init(MPZ_VAL(r));
  return r;
}

value ml_mpz_copy(value x)
{
  ALLOC_MPZ(r);
  mpz_init_set(MPZ_VAL(r), MPZ_VAL(x));
  return (value) r;
}

value ml_mpz2_copy(value r, value x)
{
  mpz_set(MPZ_VAL(r), MPZ_VAL(x));
  return r;
}

/* Binary ops (except division) */
#define MPZ_l2(func) \
value ml_mpz_##func (value x, value y) \
{ \
  value r=alloc_init_mpz(); \
  mpz_##func (MPZ_VAL(r), MPZ_VAL(x), MPZ_VAL(y)); \
  return (value) r; \
} \
\
value ml_mpz2_##func (value r, value x, value y) \
{ \
  mpz_##func (MPZ_VAL(r), MPZ_VAL(x), MPZ_VAL(y)); \
  return r; \
}

MPZ_l2(and)
MPZ_l2(ior)

#define MPZ_u2(func) \
value ml_mpz_##func (value x, value y) \
{ \
  value r=alloc_init_mpz(); \
  mpz_##func (MPZ_VAL(r), MPZ_VAL(x), Long_val(y)); \
  return (value) r; \
} \
\
value ml_mpz2_##func (value r, value x, value y) \
{ \
  mpz_##func (MPZ_VAL(r), MPZ_VAL(x), Long_val(y)); \
  return r; \
}

#define MPZ_2(func) \
MPZ_l2(func) \
MPZ_u2(func##_ui)

MPZ_2(add)
MPZ_2(sub)
MPZ_2(mul)

/* Exponentiation */
MPZ_u2(pow_ui)

value ml_mpz_ui_pow_ui(value x, value y)
{
  value r = alloc_init_mpz();
  mpz_ui_pow_ui(MPZ_VAL(r), Long_val(x), Long_val(y));
  return (value) r;
}

value ml_mpz2_ui_pow_ui(value r, value x, value y)
{
  mpz_ui_pow_ui(MPZ_VAL(r), Long_val(x), Long_val(y));
  return r;
}

value ml_mpz_powm(value base, value exp, value mod)
{
  value r = alloc_init_mpz();
  mpz_powm(MPZ_VAL(r), MPZ_VAL(base), MPZ_VAL(exp), MPZ_VAL(mod));
  return (value) r;
}

value ml_mpz2_powm(value r, value base, value exp, value mod)
{
  mpz_powm(MPZ_VAL(r), MPZ_VAL(base), MPZ_VAL(exp), MPZ_VAL(mod));
  return r;
}

value ml_mpz_powm_ui(value base, value exp, value mod)
{
  value r = alloc_init_mpz();
  mpz_powm_ui(MPZ_VAL(r), MPZ_VAL(base), Long_val(exp), MPZ_VAL(mod));
  return (value) r;
}

value ml_mpz2_powm_ui(value r, value base, value exp, value mod)
{
  mpz_powm_ui(MPZ_VAL(r), MPZ_VAL(base), Long_val(exp), MPZ_VAL(mod));
  return r;
}

value ml_mpz2_setbit(value r, value exp)
{
  mpz_setbit(MPZ_VAL(r), Long_val(exp));
  return r;
}

value ml_mpz2_clrbit(value r, value exp)
{
  mpz_clrbit(MPZ_VAL(r), Long_val(exp));
  return r;
}

/* Unary ops */
#define MPZ_1(func) \
value ml_mpz_##func (value x) \
{ \
  value r = alloc_init_mpz(); \
  mpz_##func (MPZ_VAL(r), MPZ_VAL(x)); \
  return (value) r; \
} \
\
value ml_mpz2_##func (value r, value x) \
{ \
  mpz_##func (MPZ_VAL(r), MPZ_VAL(x)); \
  return r; \
}

MPZ_1(neg)
MPZ_1(abs)
MPZ_1(com)

/* Square roots */
MPZ_1(sqrt)

value ml_mpz_sqrtrem(value x)
{
  value r1 = alloc_init_mpz(), r2 = alloc_init_mpz();
  value ret;
  mpz_sqrtrem(MPZ_VAL(r1), MPZ_VAL(r2), MPZ_VAL(x));
  ret = alloc(2, 0);
  Field(ret, 0) = (value) r1;
  Field(ret, 1) = (value) r2;
  return ret;
}

value ml_mpz_perfect_square_p(value x)
{
  return mpz_perfect_square_p(MPZ_VAL(x)) ? Val_true : Val_false;
}

/* Misc */
value ml_mpz_mul2exp(value x, value shift)
{
  value r = alloc_init_mpz();
  mpz_mul_2exp(MPZ_VAL(r), MPZ_VAL(x), Long_val(shift));
  return (value) r;
}

value ml_mpz2_mul2exp(value r, value x, value shift)
{
  mpz_mul_2exp(MPZ_VAL(r), MPZ_VAL(x), Long_val(shift));
  return r;
}

#define MPZ_u1(func) \
value ml_mpz_##func (value x) \
{ \
  value r = alloc_init_mpz(); \
  mpz_##func (MPZ_VAL(r), Long_val(x)); \
  return (value) r; \
} \
\
value ml_mpz2_##func (value r, value x) \
{ \
  mpz_##func (MPZ_VAL(r), Long_val(x)); \
  return r; \
}

MPZ_u1(fac_ui)
MPZ_u1(random)
MPZ_u1(random2)

/* Division */
#define MPZ_l2_p(func) \
value ml_mpz_##func (value x, value y) \
{ \
  value r1 = alloc_init_mpz(), r2 = alloc_init_mpz(); \
  value ret; \
  mpz_##func(MPZ_VAL(r1), MPZ_VAL(r2), MPZ_VAL(x), MPZ_VAL(y)); \
  ret = alloc(2, 0); \
  Field(ret, 0) = (value) r1; \
  Field(ret, 1) = (value) r2; \
  return ret; \
}

#define MPZ_2_p(func) \
MPZ_l2_p(func) \
value ml_mpz_##func##_ui (value x, value y) \
{ \
  value r1 = alloc_init_mpz(), r2 = alloc_init_mpz(); \
  value ret; \
  mpz_##func##_ui (MPZ_VAL(r1), MPZ_VAL(r2), MPZ_VAL(x), Long_val(y)); \
  ret = alloc(2, 0); \
  Field(ret, 0) = (value) r1; \
  Field(ret, 1) = (value) r2; \
  return ret; \
}

#define MPZ_div(letter) \
  MPZ_2(letter##div_q) \
  MPZ_2(letter##div_r) \
  MPZ_2_p(letter##div_qr)

#define MPZ_div2exp(letter) \
  MPZ_u2(letter##div_q_2exp) \
  MPZ_u2(letter##div_r_2exp) \

MPZ_div(t)
MPZ_div(f)
MPZ_div(c)

MPZ_div2exp(t)
MPZ_div2exp(f)

MPZ_2(mod)
MPZ_l2(divexact)

/* Compare */
#define MPZ_il2(func) \
value ml_mpz_##func (value x, value y) \
{ \
  return Val_int(mpz_##func (MPZ_VAL(x), \
    MPZ_VAL(y))); \
}

#define MPZ_iu2(func) \
value ml_mpz_##func (value x, value y) \
{ \
  return Val_int(mpz_##func (MPZ_VAL(x), Long_val(y))); \
}

#define MPZ_i2(func) \
MPZ_il2(func) \
MPZ_iu2(func##_ui) 

MPZ_il2(cmp)
MPZ_iu2(cmp_si)

#define MPZ_i1(func) \
value ml_mpz_##func (value x) \
{ \
  return Val_int(mpz_##func (MPZ_VAL(x))); \
}

MPZ_i1(sgn)

MPZ_i1(popcount)
MPZ_il2(hamdist)

MPZ_iu2(scan0)
MPZ_iu2(scan1)

/* Number theoretic */
MPZ_2(gcd)

value ml_mpz_probab_prime_p(value x, value y)
{
  return mpz_probab_prime_p(MPZ_VAL(x), Int_val(y)) ? Val_true : Val_false;
}

value ml_mpz_gcdext(value x, value y)
{
  value r;
  value r1 = alloc_init_mpz(), r2 = alloc_init_mpz(),
        r3 = alloc_init_mpz();
  mpz_gcdext(MPZ_VAL(r1), MPZ_VAL(r2), MPZ_VAL(r3), MPZ_VAL(x), MPZ_VAL(y));
  r=alloc(3, 0);
  Field(r,0) = (value) r1;
  Field(r,1) = (value) r2;
  Field(r,2) = (value) r3;
  return r;
}

MPZ_il2(jacobi)
MPZ_il2(legendre)

value ml_mpz_invert(value x, value mod)
{
  value r = alloc_init_mpz();
  if (! mpz_invert(MPZ_VAL(r), MPZ_VAL(x), MPZ_VAL(mod)))
    failwith("Gmp.Z.invert");
  return (value) r;
}

value ml_mpz2_invert(value r, value x, value mod)
{
  if (! mpz_invert(MPZ_VAL(r), MPZ_VAL(x), MPZ_VAL(mod)))
    failwith("Gmp.Z2.invert");
  return r;
}

/******************************* MPQ *******************************/
#define MPQ_VAL(x) (*((mpq_t*)(Data_custom_val(x))))

static void ml_mpq_finalize(value);
static int ml_mpq_compare(value, value);
static long ml_mpq_hash(value);

static struct custom_operations mpq_t_custom_operations = {
  "GMP/3.1/q",
  ml_mpq_finalize,
  ml_mpq_compare,
  ml_mpq_hash,
  custom_serialize_default,
  custom_deserialize_default
};

void ml_mpq_finalize(value v) {
  mpq_clear(MPQ_VAL(v));
}

int ml_mpq_compare(value v1, value v2) {
  return mpq_cmp(MPQ_VAL(v1), MPQ_VAL(v2));
}

long ml_mpq_hash(value v) {
  value n = alloc_init_mpz();
  mpq_get_num(MPZ_VAL(n),MPQ_VAL(v));
  return mpz_get_ui(MPZ_VAL(n));
}

/* Alloc */
value alloc_init_mpq()
{
  value v = alloc_custom(&mpq_t_custom_operations,sizeof(mpq_t),0,1);
  mpq_init(MPQ_VAL(v));
  return v;
}

value ml_mpq_from_ints(value x, value y)
{
  value r = alloc_init_mpq();
  mpq_set_si(MPQ_VAL(r), Long_val(x), Long_val(y));
  mpq_canonicalize(MPQ_VAL(r));
  return (value) r;
}

value ml_mpq2_from_ints(value r, value x, value y)
{
  mpq_set_si(MPQ_VAL(r), Long_val(x), Long_val(y));
  mpq_canonicalize(MPQ_VAL(r));
  return r;
}

value ml_mpq_from_z(value x)
{
  value r = alloc_init_mpq();
  mpq_set_z(MPQ_VAL(r), MPZ_VAL(x));
  mpq_canonicalize(MPQ_VAL(r));
  return (value) r;
}

value ml_mpq2_from_z(value r, value x)
{
  mpq_set_z(MPQ_VAL(r), MPZ_VAL(x));
  mpq_canonicalize(MPQ_VAL(r));
  return r;
}

value ml_float_from_mpq(value v)
{
  value ret=alloc(1, Double_tag);
  Store_double_val(ret, mpq_get_d(MPQ_VAL(v)));
  return ret;
}

value ml_mpq_copy(value x)
{
  value r = alloc_init_mpq();
  mpq_set(MPQ_VAL(r), MPQ_VAL(x));
  return (value) r;
}

value ml_mpq2_copy(value r, value x)
{
  mpq_set(MPQ_VAL(r), MPQ_VAL(x));
  return r;
}

#define MPQ_2(func) \
value ml_mpq_##func (value x, value y) \
{ \
  value r = alloc_init_mpq(); \
  mpq_##func (MPQ_VAL(r), MPQ_VAL(x), \
    MPQ_VAL(y)); \
  return (value) r; \
} \
\
value ml_mpq2_##func (value r, value x, value y) \
{ \
  mpq_##func (MPQ_VAL(r), MPQ_VAL(x), MPQ_VAL(y)); \
  return r; \
}

MPQ_2(add)
MPQ_2(sub)
MPQ_2(mul)
MPQ_2(div)

#define MPQ_1(func) \
value ml_mpq_##func (value x) \
{ \
  value r = alloc_init_mpq(); \
  mpq_##func (MPQ_VAL(r), MPQ_VAL(x) ); \
  return (value) r; \
} \
\
value ml_mpq2_##func (value r, value x) \
{ \
  mpq_##func (MPQ_VAL(r), MPQ_VAL(x) ); \
  return r; \
}

MPQ_1(neg)
MPQ_1(inv)

#define MPQZ_1(func) \
value ml_mpq_##func (value x) \
{ \
  value r = alloc_init_mpz(); \
  mpq_##func (MPZ_VAL(r), MPQ_VAL(x) ); \
  return (value) r; \
} \
\
value ml_mpq2_##func (value r, value x) \
{ \
  mpq_##func (MPZ_VAL(r), MPQ_VAL(x) ); \
  return r; \
}

MPQZ_1(get_num)
MPQZ_1(get_den)

#define MPQ_i2(func) \
value ml_mpq_##func (value x, value y) \
{ \
  return Val_int(mpq_##func (MPQ_VAL(x), MPQ_VAL(y)) ); \
}

MPQ_i2(cmp)

#define MPQ_i1(func) \
value ml_mpq_##func (value x) \
{ \
  return Val_int(mpq_##func (MPQ_VAL(x) )); \
}

MPQ_i1(sgn)

value ml_mpq_equal (value x, value y)
{
  /* mpq_equal has a BUG! */
  return mpq_cmp(MPQ_VAL(x), MPQ_VAL(y)) ? Val_false : Val_true;
}

