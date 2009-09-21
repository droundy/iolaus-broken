#ifdef GADT_WITNESSES

#define C(contexts) contexts
#define FORALL(types) forall types.

#else

#define C(contexts)
#define FORALL(types)

#endif
