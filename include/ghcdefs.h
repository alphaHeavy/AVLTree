#define UINT Int#
#define COMPAREUINT compareInt#
#define INCINT1(n) ((n)+#1#)
#define INCINT2(n) ((n)+#2#)
#define INCINT3(n) ((n)+#3#)
#define INCINT4(n) ((n)+#4#)
#define DECINT1(n) ((n)-#1#)
#define DECINT2(n) ((n)-#2#)
#define DECINT3(n) ((n)-#3#)
#define DECINT4(n) ((n)-#4#)
#define SUBINT(m,n) ((m)-#(n))
#define ADDINT(m,n) ((m)+#(n))
#define L(n) n#
#define LEQ <=#
#define LTN <#
#define EQL ==#
#define ASINT(n) (I# (n))
#define NEGATE(n) (0#-#(n))
#define _MODULO_(n,m) (modInt# n m)
#define UBT2(y,z) (# y,z #)
#define UBT3(x,y,z) (# x,y,z #)
#define UBT4(w,x,y,z) (# w,x,y,z #)
#define UBT5(v,w,x,y,z) (# v,w,x,y,z #)
#define UBT6(u,v,w,x,y,z) (# u,v,w,x,y,z #)
#define IS_TRUE(bi) (isTrue# (bi))
#define IS_NEG(n) IS_TRUE(n <# 0#)
#define LEFT_JUSTIFY_INT(m,n) (iShiftL# (m) (32#-#n))
