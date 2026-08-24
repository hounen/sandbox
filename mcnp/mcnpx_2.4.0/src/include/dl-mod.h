/*
 * sym = get_module_skeleton (module_name, symbol);
 * if (sym)
 *      (*sym)(args...);
 */
typedef void *Function ();

extern Function *get_module_skeleton (char *, char *);

/*
 * if (load_module (module_name))
 *      do stuff with module loaded...
 */
extern void *load_module (char *);
