/*
 * ML GMP - Interface between Objective Caml and GNU MP
 * Copyright (C) 2001 David MONNIAUX
 * 
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License version 2 published by the Free Software Foundation,
 * or any more recent version published by the Free Software
 * Foundation, at your choice.
 * 
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * 
 * See the GNU Library General Public License version 2 for more details
 * (enclosed in the file LGPL).
 *
 * As a special exception to the GNU Library General Public License, you
 * may link, statically or dynamically, a "work that uses the Library"
 * with a publicly distributed version of the Library to produce an
 * executable file containing portions of the Library, and distribute
 * that executable file under terms of your choice, without any of the
 * additional requirements listed in clause 6 of the GNU Library General
 * Public License.  By "a publicly distributed version of the Library",
 * we mean either the unmodified Library as distributed by INRIA, or a
 * modified version of the Library that is distributed under the
 * conditions defined in clause 3 of the GNU Library General Public
 * License.  This exception does not however invalidate any other reasons
 * why the executable file might be covered by the GNU Library General
 * Public License.
 */

/* deprecated to modules [Z] and [Q] and consolidated in one file. Also
   tracing has been removed. 

   Harald Ruess */


#include <caml/mlvalues.h>
#include <caml/custom.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>


/**** from config.h ****/


#define SERIALIZE
#define USE_MPFR
#define NDEBUG

#include <gmp.h>

/* This is the largest prime less than 2^32 */
#define HASH_MODULUS 4294967291UL

#ifdef __GNUC__
#define noreturn __attribute__((noreturn))
#else
#define noreturn
#endif

/* In C99 or recent versions of gcc,
   - you can specify which field you want to initialize
   - you have "inline". */

#if defined(__GNUC__) || (defined(__STDC__) && __STDC_VERSION__ >= 199901L)
#define field(x) .x =
#else
#define field(x)
#define inline
#endif

#ifdef SERIALIZE
/* Sizes of types on arch 32/ arch 64 */

/* THOSE SIZES ARE A HACK. */

/* __mpz_struct = 2*int + ptr */
#define MPZ_SIZE_ARCH32 12
#define MPZ_SIZE_ARCH64 16

/* __mpq_struct = 2 * __mpz_struct */
#define MPQ_SIZE_ARCH32 (2 * MPZ_SIZE_ARCH32)
#define MPQ_SIZE_ARCH64 (2 * MPZ_SIZE_ARCH64)

/* __mpf_struct = 3 * int + ptr */
#define MPF_SIZE_ARCH32 16
#define MPF_SIZE_ARCH64 24

/* __mpfr_struct = 3 * int + ptr */
#define MPFR_SIZE_ARCH32 16
#define MPFR_SIZE_ARCH64 24

extern void serialize_int_4(int32 i);
extern void serialize_block_1(void * data, long len);

extern uint32 deserialize_uint_4(void);
extern int32 deserialize_sint_4(void);
extern void deserialize_block_1(void * data, long len);

#endif /* SERIALIZE */


/**** from mlgmp_misc.h ****/

void division_by_zero(void) {
  raise_constant(*caml_named_value("Gmp.Division_by_zero"));
}


/*** from conversions.c ***/


struct custom_operations _mlgmp_custom_z;

static inline gmp_randstate_t *randstate_val(value val)
{
  return ((gmp_randstate_t *) (Data_custom_val(val)));
}

static inline int Int_option_val(value val, int default_val)
{
  if (val == Val_int(0)) return default_val;
  return Int_val(Field(val, 0));
}

static inline mpz_t * mpz_val (value val)
{
  return ((mpz_t *) (Data_custom_val(val)));
}

static inline value alloc_mpz (void)
{
  return alloc_custom(&_mlgmp_custom_z,
		       sizeof(mpz_t),
		       0,
		       1);
}

static inline value alloc_init_mpz (void)
{
  value r= alloc_mpz();
  mpz_init(*mpz_val(r));
  return r;
}

#pragma inline(Int_option_val, mpz_val, alloc_mpz, alloc_init_mpz)

struct custom_operations _mlgmp_custom_q;

static inline mpq_t * mpq_val (value val)
{
  return ((mpq_t *) (Data_custom_val(val)));
}

static inline value alloc_mpq (void)
{
  return alloc_custom(&_mlgmp_custom_q,
		       sizeof(mpq_t),
		       0,
		       1);
}

static inline value alloc_init_mpq (void)
{
  value r= alloc_mpq();
  mpq_init(*mpq_val(r));
  return r;
}

#pragma inline(mpq_val, alloc_mpq, alloc_init_mpq)


struct custom_operations _mlgmp_custom_f;

static inline mpf_t * mpf_val (value val)
{
  return ((mpf_t *) (Data_custom_val(val)));
}

static inline value alloc_mpf (void)
{
  return alloc_custom(&_mlgmp_custom_f,
		       sizeof(mpf_t),
		       0,
		       1);
}

static inline value alloc_init_mpf (value prec)
{
  value r= alloc_mpf();
  mpf_init2(*mpf_val(r), Int_val(prec));
  return r;
}

struct custom_operations _mlgmp_custom_fr;

/*** Allocation functions */

void _mlgmp_random_finalize(value r)
{
  gmp_randclear(Data_custom_val(r));
}

struct custom_operations _mlgmp_custom_random =
  {
    field(identifier)  "Gmp.Random.randstate_t",
    field(finalize)    &_mlgmp_random_finalize,
    field(compare)     custom_compare_default,
    field(hash)        custom_hash_default,
    field(serialize)   custom_serialize_default,
    field(deserialize) custom_deserialize_default
  };


value _mlgmp_randinit_lc(value n)
{
  CAMLparam1(n);
  CAMLlocal1(r);
  r = alloc_custom(&_mlgmp_custom_random,
		       sizeof(gmp_randstate_t),
		       4,
		       1000000);
  gmp_randinit(*((gmp_randstate_t*) Data_custom_val(r)),
	       GMP_RAND_ALG_LC,
	       Int_val(n));
  CAMLreturn(r); 
}


/*** from mlgmp_z.c ***/

/*** Allocation functions */

void _mlgmp_z_finalize(value r)
{
  mpz_clear(*mpz_val(r));
}

int _mlgmp_z_custom_compare(value a, value b);
void _mlgmp_z_serialize(value v,
			unsigned long * wsize_32,
			unsigned long * wsize_64);
unsigned long _mlgmp_z_deserialize(void * dst);
long _mlgmp_z_hash(value v);

struct custom_operations _mlgmp_custom_z =
  {
    field(identifier)  "Gmp.Z.t",
    field(finalize)    &_mlgmp_z_finalize,
    field(compare)     &_mlgmp_z_custom_compare,
    field(hash)        &_mlgmp_z_hash,
#ifdef SERIALIZE
    field(serialize)   &_mlgmp_z_serialize,
    field(deserialize) &_mlgmp_z_deserialize
#else
    field(serialize)   custom_serialize_default,
    field(deserialize) custom_deserialize_default
#endif
  };

value _mlgmp_z_create(void)
{
  CAMLparam0();
  CAMLreturn(alloc_init_mpz());
}

value _mlgmp_z_copy(value from)
{
  CAMLparam1(from);
  CAMLlocal1(r);
  r = alloc_mpz();
  mpz_init_set(*mpz_val(r), *mpz_val(from));
  CAMLreturn(r);
}

value _mlgmp_z_from_int(value ml_val)
{
  CAMLparam1(ml_val);
  CAMLlocal1(r);
  r=alloc_mpz();
  mpz_init_set_si(*mpz_val(r), Int_val(ml_val));
  CAMLreturn(r);
}

value _mlgmp_z_from_string_base(value base, value ml_val)
{
  CAMLparam2(base, ml_val);
  CAMLlocal1(r);
  r=alloc_mpz();
  mpz_init_set_str(*mpz_val(r), String_val(ml_val), Int_val(base));
  CAMLreturn(r);
}

value _mlgmp_z_from_float(value ml_val)
{
  CAMLparam1(ml_val);
  CAMLlocal1(r);
  r=alloc_mpz();
  mpz_init_set_d(*mpz_val(r), Double_val(ml_val));
  CAMLreturn(r);
}

value _mlgmp_z2_from_int(value r, value ml_val)
{
  CAMLparam2(r, ml_val);
  mpz_init_set_si(*mpz_val(r), Int_val(ml_val));
  CAMLreturn(Val_unit);
}

value _mlgmp_z2_from_string_base(value r, value base, value ml_val)
{
  CAMLparam3(r, base, ml_val);
  mpz_init_set_str(*mpz_val(r), String_val(ml_val), Int_val(base));
  CAMLreturn(Val_unit);
}

value _mlgmp_z2_from_float(value r, value ml_val)
{
  CAMLparam2(r, ml_val);
  mpz_init_set_d(*mpz_val(r), Double_val(ml_val));
  CAMLreturn(Val_unit);
}

/*** Conversions */

value _mlgmp_z_to_string_base(value ml_base, value ml_val)
{
  int base;
  char *s;

  CAMLparam2(ml_base, ml_val);
  CAMLlocal1(r);
  mpz_t *val=mpz_val(ml_val);
  base=Int_val(ml_base);

  /* This is sub-optimal, but using mpz_sizeinbase would
     need a means of shortening the length of a pre-allocated
     Caml string (mpz_sizeinbase sometimes overestimates lengths). */
  s=mpz_get_str(NULL, base, *val);
  r=alloc_string(strlen(s));
  strcpy(String_val(r), s);
  free(s);

  CAMLreturn(r);
}

value _mlgmp_z_to_int(value ml_val)
{
  CAMLparam1(ml_val);
  CAMLreturn(Val_int(mpz_get_si(* mpz_val(ml_val))));
}

value _mlgmp_z_to_float(value v)
{
  CAMLparam1(v);
  CAMLlocal1(r);
  r = copy_double(mpz_get_d(*mpz_val(v)));
  CAMLreturn(r);
}

/*** Operations */
/**** Arithmetic */

#define z_binary_op_ui(op)                              \
value _mlgmp_z_##op(value a, value b)		        \
{							\
  CAMLparam2(a, b);                                     \
  CAMLlocal1(r);                                        \
  r=alloc_init_mpz();				        \
  mpz_##op(*mpz_val(r), *mpz_val(a), Int_val(b));	\
  CAMLreturn(r);					\
}                                                       \
                                                        \
value _mlgmp_z2_##op(value r, value a, value b)		\
{							\
  CAMLparam3(r, a, b);                                  \
  mpz_##op(*mpz_val(r), *mpz_val(a), Int_val(b));	\
  CAMLreturn(Val_unit);					\
}

#define z_binary_op_mpz(op)				\
value _mlgmp_z_##op(value a, value b)			\
{							\
  CAMLparam2(a, b);                                     \
  CAMLlocal1(r);                                        \
  r=alloc_init_mpz();				        \
  mpz_##op(*mpz_val(r), *mpz_val(a), *mpz_val(b));	\
  CAMLreturn(r);	       				\
}                                                       \
                                                        \
value _mlgmp_z2_##op(value r, value a, value b)	       	\
{							\
  CAMLparam3(r, a, b);                                  \
  mpz_##op(*mpz_val(r), *mpz_val(a), *mpz_val(b));	\
  CAMLreturn(Val_unit);	       				\
}

#define z_binary_op(op)				\
z_binary_op_mpz(op)				\
z_binary_op_ui(op##_ui)

z_binary_op(add)
z_binary_op(sub)
z_binary_op(mul)

/**** Powers */
z_binary_op_ui(pow_ui)


value _mlgmp_z_powm_ui(value a, value b, value modulus)
{
  CAMLparam3(a, b, modulus);
  CAMLlocal1(r);
  r=alloc_init_mpz();
  mpz_powm_ui(*mpz_val(r), *mpz_val(a), Int_val(b), *mpz_val(modulus));
  CAMLreturn(r);
}

value _mlgmp_z_ui_pow_ui(value a, value b)
{
  CAMLparam2(a, b);
  CAMLlocal1(r);
  r=alloc_init_mpz();
  mpz_ui_pow_ui(*mpz_val(r), Int_val(a), Int_val(b));
  CAMLreturn(r);
}

value _mlgmp_z_powm(value a, value b, value modulus)
{
  CAMLparam3(a, b, modulus);
  CAMLlocal1(r);
  r=alloc_init_mpz();
  mpz_powm(*mpz_val(r), *mpz_val(a), *mpz_val(b), *mpz_val(modulus));
  CAMLreturn(r);
}

value _mlgmp_z2_powm_ui(value r, value a, value b, value modulus)
{
  CAMLparam4(r, a, b, modulus);
  mpz_powm_ui(*mpz_val(r), *mpz_val(a), Int_val(b), *mpz_val(modulus));
  CAMLreturn(Val_unit);
}

value _mlgmp_z2_ui_pow_ui(value r, value a, value b)
{
  CAMLparam3(r, a, b);
  mpz_ui_pow_ui(*mpz_val(r), Int_val(a), Int_val(b));
  CAMLreturn(Val_unit);
}

value _mlgmp_z2_powm(value r, value a, value b, value modulus)
{
  CAMLparam4(r, a, b, modulus);
  mpz_powm(*mpz_val(r), *mpz_val(a), *mpz_val(b), *mpz_val(modulus));
  CAMLreturn(Val_unit);
}

/**** Unary */
#define z_unary_op(op)				\
value _mlgmp_z_##op(value a)			\
{						\
  CAMLparam1(a);				\
  CAMLlocal1(r);				\
  r=alloc_init_mpz();   			\
  mpz_##op(*mpz_val(r), *mpz_val(a));		\
  CAMLreturn(r);				\
}                                               \
                                                \
value _mlgmp_z2_##op(value r, value a)	        \
{						\
  CAMLparam2(r, a);				\
  mpz_##op(*mpz_val(r), *mpz_val(a));		\
  CAMLreturn(Val_unit);				\
}

z_unary_op(neg)
z_unary_op(abs)

/**** Roots */

/* Negative ?*/
z_unary_op(sqrt)

value _mlgmp_z_sqrtrem(value a)
{
  CAMLparam1(a);
  CAMLlocal3(q, r, qr);
  q=alloc_init_mpz();
  r=alloc_init_mpz();

  mpz_sqrtrem(*mpz_val(q), *mpz_val(r), *mpz_val(a));

  qr=alloc_tuple(2);
  Store_field(qr, 0, q);
  Store_field(qr, 1, r);
  CAMLreturn(qr);
}
									
z_binary_op_ui(root)

#define z_unary_p(name)				\
value _mlgmp_z_##name(value a)			\
{						\
  CAMLparam1(a);                                \
  CAMLreturn(Val_bool(mpz_##name(*mpz_val(a))));\
}

z_unary_p(perfect_power_p)
z_unary_p(perfect_square_p)

/**** Division */
#define z_xdivision_op(kind)						\
value _mlgmp_z_##kind##div_qr(value n, value d)				\
{									\
  CAMLparam2(n, d);							\
  CAMLlocal3(q, r, qr);							\
  mpz_t *mpz_d = mpz_val(d);						\
									\
  if (! mpz_sgn(*mpz_d))						\
    division_by_zero();							\
									\
  q=alloc_init_mpz();							\
  r=alloc_init_mpz();							\
									\
  mpz_##kind##div_qr(*mpz_val(q), *mpz_val(r), *mpz_val(n), *mpz_d);	\
									\
  qr=alloc_tuple(2);							\
  Store_field(qr, 0, q);						\
  Store_field(qr, 1, r);						\
  CAMLreturn(qr);						        \
}									\
									\
value _mlgmp_z_##kind##div_q(value n, value d)				\
{									\
  CAMLparam2(n, d);                                                     \
  CAMLlocal1(q);						      	\
  mpz_t *mpz_d = mpz_val(d);						\
									\
  if (! mpz_sgn(*mpz_d))						\
    division_by_zero();							\
									\
  q=alloc_init_mpz();							\
									\
  mpz_##kind##div_q(*mpz_val(q), *mpz_val(n), *mpz_d);			\
									\
  CAMLreturn(q);	       						\
}									\
									\
value _mlgmp_z2_##kind##div_q(value q, value n, value d)		\
{									\
  CAMLparam3(q, n, d);                                                  \
  mpz_t *mpz_d = mpz_val(d);						\
									\
  if (! mpz_sgn(*mpz_d))						\
    division_by_zero();							\
									\
  mpz_##kind##div_q(*mpz_val(q), *mpz_val(n), *mpz_d);			\
									\
  CAMLreturn(Val_unit);	       						\
}									\
									\
value _mlgmp_z_##kind##div_r(value n, value d)				\
{									\
  CAMLparam2(n, d);                                                     \
  CAMLlocal1(r);						      	\
  mpz_t *mpz_d = mpz_val(d);						\
									\
  if (! mpz_sgn(*mpz_d))						\
    division_by_zero();							\
									\
  r=alloc_init_mpz();							\
									\
  mpz_##kind##div_r(*mpz_val(r), *mpz_val(n), *mpz_d);			\
									\
  CAMLreturn(r);	       						\
}									\
									\
value _mlgmp_z2_##kind##div_r(value r, value n, value d)      		\
{									\
  CAMLparam3(r, n, d);                                                     \
  mpz_t *mpz_d = mpz_val(d);						\
									\
  if (! mpz_sgn(*mpz_d))						\
    division_by_zero();							\
									\
  mpz_##kind##div_r(*mpz_val(r), *mpz_val(n), *mpz_d);			\
									\
  CAMLreturn(Val_unit);	       						\
}									\
									\
value _mlgmp_z_##kind##div_qr_ui(value n, value d)			\
{									\
  CAMLparam2(n, d);                                                     \
  CAMLlocal3(q, r, qr);							\
  unsigned long int ui_d = Int_val(d);					\
									\
  if (! ui_d) division_by_zero();					\
									\
  q=alloc_init_mpz();							\
  r=alloc_init_mpz();							\
									\
  mpz_##kind##div_qr_ui(*mpz_val(q), *mpz_val(r), *mpz_val(n), ui_d);	\
									\
  qr=alloc_tuple(2);							\
  Store_field(qr, 0, q);						\
  Store_field(qr, 1, r);						\
  CAMLreturn(qr);	       						\
}									\
									\
value _mlgmp_z_##kind##div_q_ui(value n, value d)			\
{									\
  CAMLparam2(n, d);                                                     \
  CAMLlocal1(q);       							\
  unsigned long int ui_d = Int_val(d);					\
									\
 if (! ui_d) division_by_zero();					\
									\
  q=alloc_init_mpz();							\
									\
  mpz_##kind##div_q_ui(*mpz_val(q), *mpz_val(n), ui_d);			\
									\
  CAMLreturn(q);	       						\
}									\
									\
value _mlgmp_z2_##kind##div_q_ui(value q, value n, value d)		\
{									\
  CAMLparam3(q, n, d);                                                     \
  unsigned long int ui_d = Int_val(d);					\
									\
 if (! ui_d) division_by_zero();					\
									\
  mpz_##kind##div_q_ui(*mpz_val(q), *mpz_val(n), ui_d);			\
									\
  CAMLreturn(Val_unit);	       						\
}									\
									\
value _mlgmp_z_##kind##div_r_ui(value n, value d)			\
{									\
  CAMLparam2(n, d);                                                     \
  CAMLlocal1(r);       							\
  unsigned long int ui_d = Int_val(d);					\
									\
  if (! ui_d) division_by_zero();					\
									\
  r=alloc_init_mpz();							\
									\
  mpz_##kind##div_r_ui(*mpz_val(r), *mpz_val(n), ui_d);			\
									\
  CAMLreturn(r);	       						\
}									\
									\
value _mlgmp_z2_##kind##div_r_ui(value r, value n, value d)		\
{									\
  CAMLparam3(r, n, d);                                                  \
  unsigned long int ui_d = Int_val(d);					\
									\
 if (! ui_d) division_by_zero();					\
									\
  mpz_##kind##div_r_ui(*mpz_val(r), *mpz_val(n), ui_d);			\
									\
  CAMLreturn(Val_unit);	       						\
}									\
									\
value _mlgmp_z_##kind##div_ui(value n, value d)				\
{									\
  CAMLparam2(n, d);                                                     \
  unsigned long int ui_d = Int_val(d);					\
									\
  if (! ui_d) division_by_zero();					\
									\
  CAMLreturn(Val_int(mpz_##kind##div_ui(*mpz_val(n), ui_d)));	        \
}

z_xdivision_op(t)
z_xdivision_op(f)
z_xdivision_op(c)

#define z_division_op(op)			\
value _mlgmp_z_##op(value n, value d)		\
{						\
  CAMLparam2(n, d);				\
  CAMLlocal1(q);				\
  mpz_t *mpz_d = mpz_val(d);			\
						\
  if (! mpz_sgn(*mpz_d))			\
    division_by_zero();				\
						\
  q=alloc_init_mpz();				\
						\
  mpz_##op(*mpz_val(q), *mpz_val(n), *mpz_d);	\
						\
  CAMLreturn(q);				\
}						\
						\
value _mlgmp_z2_##op(value q, value n, value d)	\
{						\
  CAMLparam3(q, n, d);				\
  mpz_t *mpz_d = mpz_val(d);			\
						\
  if (! mpz_sgn(*mpz_d))			\
    division_by_zero();				\
						\
  mpz_##op(*mpz_val(q), *mpz_val(n), *mpz_d);	\
						\
  CAMLreturn(Val_unit);				\
}

#define z_division_op_ui(op)			\
value _mlgmp_z_##op(value n, value d)		\
{						\
  CAMLparam2(n, d);				\
  CAMLlocal1(q);				\
  unsigned int ld = Int_val(d);			\
						\
  if (! ld)	                 		\
    division_by_zero();				\
						\
  q=alloc_init_mpz();				\
						\
  mpz_##op(*mpz_val(q), *mpz_val(n), ld);	\
						\
  CAMLreturn(q);				\
}						\
						\
value _mlgmp_z2_##op(value q, value n, value d)	\
{						\
  CAMLparam3(q, n, d);				\
  unsigned int ld = Int_val(d);			\
						\
  if (! ld)			                \
    division_by_zero();				\
						\
  mpz_##op(*mpz_val(q), *mpz_val(n), ld);	\
						\
  CAMLreturn(Val_unit);				\
}

z_division_op(divexact)
z_division_op(mod)
z_division_op_ui(mod_ui)

/*** Shift ops */
#define z_shift_op(type)				\
value _mlgmp_z_##type(value a, value shift)		\
{                                                       \
  CAMLparam2(a, shift);                                 \
  CAMLlocal1(r);					\
  r=alloc_init_mpz();   				\
  mpz_##type(*mpz_val(r), *mpz_val(a), Int_val(shift));	\
  CAMLreturn(r);       					\
}                                                       \
                                                        \
value _mlgmp_z2_##type(value r, value a, value shift)	\
{                                                       \
  CAMLparam3(r, a, shift);                              \
  mpz_##type(*mpz_val(r), *mpz_val(a), Int_val(shift));	\
  CAMLreturn(Val_unit);     				\
}

#define z_shift_op_unimplemented(type)			\
value _mlgmp_z_##type(value a, value shift)		\
{                                                       \
  CAMLparam2(a, shift);                                 \
  CAMLreturn0();       					\
}                                                       \
                                                        \
value _mlgmp_z2_##type(value r, value a, value shift)	\
{                                                       \
  CAMLparam3(r, a, shift);                              \
  unimplemented(z2_##type);                             \
  CAMLreturn0();     				        \
}

z_shift_op(mul_2exp)
z_shift_op(tdiv_q_2exp)
z_shift_op(tdiv_r_2exp)
z_shift_op(fdiv_q_2exp)
z_shift_op(fdiv_r_2exp)

#if __GNU_MP_VERSION >= 4
z_shift_op(cdiv_q_2exp)
z_shift_op(cdiv_r_2exp)
#else
z_shift_op_unimplemented(cdiv_q_2exp)
z_shift_op_unimplemented(cdiv_r_2exp)
#endif

/*** Comparisons */

int _mlgmp_z_custom_compare(value a, value b)
{
  CAMLparam2(a, b);
  CAMLreturn(mpz_cmp(*mpz_val(a), *mpz_val(b)));
}

value _mlgmp_z_compare(value a, value b)
{
  CAMLparam2(a, b);
  CAMLreturn(Val_int(mpz_cmp(*mpz_val(a), *mpz_val(b))));
}

value _mlgmp_z_compare_si(value a, value b)
{
  CAMLparam2(a, b);
  CAMLreturn(Val_int(mpz_cmp_si(*mpz_val(a), Int_val(b))));
}

/*** Number theory */

value _mlgmp_z_probab_prime_p(value n, value reps)
{
  CAMLparam2(n, reps);
  CAMLreturn(Val_bool(mpz_probab_prime_p(*mpz_val(n), Int_val(reps))));
}

z_unary_op(nextprime)

z_binary_op(gcd)
z_binary_op_mpz(lcm)

value  _mlgmp_z_gcdext(value a, value b)
{
  CAMLparam2(a, b);
  CAMLlocal4(g, s, t, r);
  g=alloc_init_mpz();  
  s=alloc_init_mpz();  
  t=alloc_init_mpz();
  mpz_gcdext(*mpz_val(g), *mpz_val(s), *mpz_val(t), *mpz_val(a), *mpz_val(b));
  r=alloc_tuple(3);
  Store_field(r, 0, g);
  Store_field(r, 1, s);
  Store_field(r, 2, t);
  CAMLreturn(r);
}						     

value  _mlgmp_z_invert(value a, value b)
{
  CAMLparam2(a, b);
  CAMLlocal2(i, r);
  i = alloc_init_mpz();
  if (! mpz_invert(*mpz_val(i),*mpz_val(a), *mpz_val(b)))
    {
      r=Val_false;
    }
  else
    {
      r=alloc_tuple(1);
      Store_field(r, 0, i);
    }
  CAMLreturn(r);
}						     

#define z_int_binary_op(op)					\
value _mlgmp_z_##op(value a, value b)				\
{								\
  CAMLparam2(a, b);						\
  CAMLreturn(Val_int(mpz_##op(*mpz_val(a), *mpz_val(b))));	\
}

z_int_binary_op(legendre)
z_int_binary_op(jacobi)

value _mlgmp_z_kronecker_si(value a, value b)
{
  CAMLparam2(a, b);
  CAMLreturn(Val_int(mpz_kronecker_si(*mpz_val(a), Int_val(b))));
}

value _mlgmp_z_si_kronecker(value a, value b)
{
  CAMLparam2(a, b);
  CAMLreturn(Val_int(mpz_si_kronecker(Int_val(a), *mpz_val(b))));
}

value _mlgmp_z_remove(value a, value b)
{
  int x;
  CAMLparam2(a, b);
  CAMLlocal2(f, r);
  f = alloc_init_mpz();
  x = mpz_remove(*mpz_val(f), *mpz_val(a), *mpz_val(b));
  r=alloc_tuple(2);
  Store_field(r, 0, f);
  Store_field(r, 1, Val_int(x));
  CAMLreturn(r);
}

#define z_unary_op_ui(op)			\
value _mlgmp_z_##op(value a)			\
{						\
  CAMLparam1(a);				\
  CAMLlocal1(r);				\
  r = alloc_init_mpz();				\
  mpz_##op(*mpz_val(r), Int_val(a));		\
  CAMLreturn(r);				\
}

z_unary_op_ui(fac_ui)
z_unary_op_ui(fib_ui)
z_binary_op_ui(bin_ui)

value _mlgmp_z_bin_uiui(value n, value k)
{
  CAMLparam2(n, k);
  CAMLlocal1(r);
  r = alloc_init_mpz();
  mpz_bin_uiui(*mpz_val(r), Int_val(n), Int_val(k));
  CAMLreturn(r);
}

#define z_int_unary_op(op)			\
value _mlgmp_z_##op(value a)			\
{						\
  CAMLparam1(a);				\
  CAMLreturn(Val_int(mpz_##op(*mpz_val(a))));	\
}

z_int_unary_op(sgn)

z_binary_op_mpz(and)
z_binary_op_mpz(ior)
z_binary_op_mpz(xor)
z_unary_op(com)

z_int_unary_op(popcount)
z_int_binary_op(hamdist)

#define z_int_binary_op_ui(op)					\
value _mlgmp_z_##op(value a, value b)				\
{								\
  CAMLparam2(a, b);						\
  CAMLreturn(Val_int(mpz_##op(*mpz_val(a), Int_val(b))));	\
}

z_int_binary_op_ui(scan0)
z_int_binary_op_ui(scan1)

/*** Random */
#define z_random_op_ui(op)					\
value _mlgmp_z_##op(value state, value n)			\
{								\
  CAMLparam2(state, n);						\
  CAMLlocal1(r);						\
  r = alloc_init_mpz();						\
  mpz_##op(*mpz_val(r), *randstate_val(state), Int_val(n));	\
  CAMLreturn(r);						\
}

#define z_random_op(op)			        		\
value _mlgmp_z_##op(value state, value n)			\
{								\
  CAMLparam2(state, n);						\
  CAMLlocal1(r);						\
  r = alloc_init_mpz();						\
  mpz_##op(*mpz_val(r), *randstate_val(state), *mpz_val(n));	\
  CAMLreturn(r);						\
}

z_random_op_ui(urandomb)
z_random_op(urandomm)
z_random_op_ui(rrandomb)

/*** Serialization */
value _mlgmp_z_initialize()
{
  CAMLparam0();
  register_custom_operations(& _mlgmp_custom_z);
  CAMLreturn(Val_unit);
}

#ifdef SERIALIZE
void _mlgmp_z_serialize(value v,
			unsigned long * wsize_32,
			unsigned long * wsize_64)
{
  CAMLparam1(v);
  char *s;
  int len;

  *wsize_32 = MPZ_SIZE_ARCH32;
  *wsize_64 = MPZ_SIZE_ARCH64;

  s = mpz_get_str (NULL, 16, *mpz_val(v));
  len = strlen(s);
  serialize_int_4(len);
  serialize_block_1(s, len);

  free(s);
  CAMLreturn0;
}

unsigned long _mlgmp_z_deserialize(void * dst)
{
  char *s;
  int len;

  len = deserialize_uint_4();
  s = malloc(len+1);
  deserialize_block_1(s, len);
  s[len] = 0;
  mpz_init_set_str (*((mpz_t*) dst), s, 16);
  free(s);

  return sizeof(mpz_t);
}
#endif

/* Hash */

long _mlgmp_z_hash(value v)
{
  CAMLparam1(v);
  mpz_t dummy;
  long r;
  mpz_init(dummy);

  r = mpz_mod_ui(dummy, *mpz_val(v), HASH_MODULUS);

  mpz_clear(dummy);
  CAMLreturn(r);
}


/*** from mlgmp_q.c ***/

#define CAMLcheckreturn(r) \
assert(r > 0x10000); \
CAMLreturn(r)

/*** Allocation functions */

void _mlgmp_q_finalize(value r)
{
  mpq_clear(*mpq_val(r));
}

int _mlgmp_q_custom_compare(value a, value b);
void _mlgmp_q_serialize(value v,
			unsigned long * wsize_32,
			unsigned long * wsize_64);
unsigned long _mlgmp_q_deserialize(void * dst);

int _mlgmp_q_custom_compare(value a, value b);
long _mlgmp_q_hash(value v);

struct custom_operations _mlgmp_custom_q =
  {
    field(identifier)  "Gmp.Q.t",
    field(finalize)    &_mlgmp_q_finalize,
    field(compare)     &_mlgmp_q_custom_compare,
    field(hash)        &_mlgmp_q_hash,
#ifdef SERIALIZE
    field(serialize)   &_mlgmp_q_serialize,
    field(deserialize) &_mlgmp_q_deserialize
#else
    field(serialize)   custom_serialize_default,
    field(deserialize) custom_deserialize_default
#endif
  };

value _mlgmp_q_create(void)
{
  CAMLparam0();
  CAMLcheckreturn(alloc_init_mpq());
}

value _mlgmp_q_from_z(value a)
{
  CAMLparam1(a);
  CAMLlocal1(r);
  r=alloc_init_mpq();
  mpq_set_z(*mpq_val(r), *mpz_val(a));
  CAMLcheckreturn(r);
}

value _mlgmp_q_from_si(value n, value d)
{
  CAMLparam2(n, d);
  CAMLlocal1(r);
  r=alloc_init_mpq();
  mpq_set_si(*mpq_val(r), Int_val(n), Int_val(d));
  mpq_canonicalize(*mpq_val(r));
  CAMLcheckreturn(r);
}

/*** Conversions */

value _mlgmp_q_from_float(value v)
{
  CAMLparam1(v);
  CAMLlocal1(r);
  r=alloc_init_mpq();
  mpq_set_d(*mpq_val(r), Double_val(v));
  CAMLcheckreturn(r);
}

value _mlgmp_q_to_float(value v)
{
  CAMLparam1(v);
  CAMLlocal1(r);
  r = copy_double(mpq_get_d(*mpq_val(v)));
  CAMLreturn(r);
}

/*** Operations */
/**** Arithmetic */

#define q_binary_op(op)	        			\
value _mlgmp_q_##op(value a, value b)			\
{							\
  CAMLparam2(a, b);                                     \
  CAMLlocal1(r);                                        \
  r=alloc_init_mpq();				        \
  mpq_##op(*mpq_val(r), *mpq_val(a), *mpq_val(b));	\
  CAMLcheckreturn(r);	       				\
}

#define q_unary_op(op)				\
value _mlgmp_q_##op(value a)			\
{						\
  CAMLparam1(a);				\
  CAMLlocal1(r);				\
  r=alloc_init_mpq();				\
  mpq_##op(*mpq_val(r), *mpq_val(a));		\
  CAMLcheckreturn(r);				\
}

q_binary_op(add)
q_binary_op(sub)
q_binary_op(mul)
q_binary_op(div)

q_unary_op(neg)
q_unary_op(inv)

#define q_z_unary_op(op)			\
value _mlgmp_q_##op(value a)			\
{						\
  CAMLparam1(a);				\
  CAMLlocal1(r);				\
  r=alloc_init_mpz();				\
  mpq_##op(*mpz_val(r), *mpq_val(a));		\
  CAMLcheckreturn(r);				\
}

q_z_unary_op(get_num)
q_z_unary_op(get_den)

/*** Compare */

int _mlgmp_q_custom_compare(value a, value b)
{
  CAMLparam2(a, b);
  CAMLreturn(mpq_cmp(*mpq_val(a), *mpq_val(b)));
}

value _mlgmp_q_cmp(value a, value b)
{
  CAMLparam2(a, b);
  CAMLreturn(Val_int(mpq_cmp(*mpq_val(a), *mpq_val(b))));
}

value _mlgmp_q_cmp_ui(value a, value n, value d)
{
  CAMLparam3(a, n, d);
  CAMLreturn(Val_int(mpq_cmp_ui(*mpq_val(a), Int_val(n), Int_val(d))));
}

value _mlgmp_q_sgn(value a)
{
  CAMLparam1(a);
  CAMLreturn(Val_int(mpq_sgn(*mpq_val(a))));
}

/*** Serialization */
value _mlgmp_q_initialize()
{
  CAMLparam0();
  register_custom_operations(& _mlgmp_custom_q);
  CAMLreturn(Val_unit);
}

#ifdef SERIALIZE
void _mlgmp_q_serialize(value v,
			unsigned long * wsize_32,
			unsigned long * wsize_64)
{
  CAMLparam1(v);
  char *s;
  int len;

  *wsize_32 = MPQ_SIZE_ARCH32;
  *wsize_64 = MPQ_SIZE_ARCH64;

  s = mpz_get_str (NULL, 16, mpq_numref(*mpq_val(v)));
  len = strlen(s);
  serialize_int_4(len);
  serialize_block_1(s, len);
  free(s);

  s = mpz_get_str (NULL, 16, mpq_denref(*mpq_val(v)));
  len = strlen(s);
  serialize_int_4(len);
  serialize_block_1(s, len);
  free(s);

  CAMLreturn0;
}

unsigned long _mlgmp_q_deserialize(void * dst)
{
  char *s;
  int len;

  len = deserialize_uint_4();
  s = malloc(len+1);
  deserialize_block_1(s, len);
  s[len] = 0;
  mpz_init_set_str (mpq_numref(*((mpq_t*) dst)), s, 16);
  free(s);

  len = deserialize_uint_4();
  s = malloc(len+1);
  deserialize_block_1(s, len);
  s[len] = 0;
  mpz_init_set_str (mpq_denref(*((mpq_t*) dst)), s, 16);
  free(s);

  return sizeof(mpq_t);
}
#endif

long _mlgmp_q_hash(value v)
{
  CAMLparam1(v);
  mpz_t dummy;
  long r;
  mpz_init(dummy);

  r = mpz_mod_ui(dummy, mpq_denref(*mpq_val(v)), HASH_MODULUS)
    ^ mpz_mod_ui(dummy, mpq_numref(*mpq_val(v)), HASH_MODULUS);

  mpz_clear(dummy);
  CAMLreturn(r);
}


/*** from mlgmp_misc.c */


value _mlgmp_get_runtime_version(value dummy)
{
  CAMLparam0();
  CAMLlocal1(r);
  r = alloc_string(strlen(gmp_version));
  strcpy(String_val(r), gmp_version);
  CAMLreturn(r);
}

value _mlgmp_get_compile_version(value dummy)
{
  CAMLparam0();
  CAMLlocal1(r);
  r = alloc_tuple(3);
  Store_field(r, 0, Val_int(__GNU_MP_VERSION));
  Store_field(r, 1, Val_int(__GNU_MP_VERSION_MINOR));
  Store_field(r, 2, Val_int(__GNU_MP_VERSION_PATCHLEVEL));
  CAMLreturn(r);
}
