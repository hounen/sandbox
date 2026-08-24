#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <fmangle.h>

#ifdef HAVE_DLFCN_H
#include <dlfcn.h>
#endif

#ifdef HAVE_DL_H
#include <dl.h>
#endif

/*
 * Variable argument list hacks...
 */
#if defined(LINUX)
#define va_copy __va_copy
#endif

#if defined(SGI) || defined(HPUX) || defined(DEC)
#define va_copy(a, b) (a) = (b)
#endif

/*
 * Dynamic shared library hacks...
 * Define our internal dso_handle: the link to a loaded shared library.
 */
#ifdef HPUX
typedef shl_t dso_handle;
#else
typedef void *dso_handle;
#endif

/*
 * A doubly-linked list of loaded modules.
 * Used to do a linear search for a module.
 * The module name is the 'alias' for the module in load_module.
 */
typedef struct mod
	{
	struct mod *next;
	struct mod *last;
	char *name;
	dso_handle handle;
	} module;

static module *modules = NULL;
static module *last_module = NULL;
char *last_error = "";
static int main_argc;
static char **main_argv;
static char **main_envp;

void init_module_system (int argc, char **argv, char **env)
	{
	main_argc = argc;
	main_argv = argv;
	main_envp = env;
	}

dso_handle dso_load (char *name)
	{
	dso_handle handle;
#ifdef HPUX
        handle = shl_load (name, BIND_IMMEDIATE|BIND_NONFATAL, 0L);
#else
        handle = dlopen (name, RTLD_LAZY | RTLD_GLOBAL);
#endif
	return (handle);
	}

void *dso_sym (dso_handle handle, char *name)
	{
#ifdef HPUX
	void *symbol;
	int status;
	extern int errno;

	errno = 0;
	status = shl_findsym (&handle, name,TYPE_PROCEDURE, &symbol);
	if (status == -1 && errno == 0)
		status = shl_findsym (&handle, name,TYPE_DATA, &symbol);
	if (status == -1)
		return (NULL);
	else
		return (symbol);
#else
	return (dlsym(handle, name));
#endif
	}

char *dso_error (void)
	{
#ifdef HPUX
	extern int errno;
	return (strerror (errno));
#else
	return (dlerror());
#endif
	}

void do_error (char *s)
	{
	fputs (s, stderr);
	fputs (": ", stderr);
	fputs (last_error, stderr);
	fputs ("\n", stderr);
	exit (1);
	}

/*
 * find_module is used to get references to loaded modules.
 * The module alias is used to see if the module is loaded.
 */

static module *find_module (char *name)
	{
	module *mp;
	
	for (mp = modules; mp; mp = mp->next)
		{
		if (strcmp (name, mp->name) == 0)
			return (mp);
		}
	return (NULL);
	}

/*
 * Now for the interfaces that actually call things in modules.
 * interface void *get_module_skeleton (char *, char *)
 * get_module_skeleton is the main way external code locates a
 * function in another module.
 */

static void *get_module_skeleton_internal (dso_handle handle, char *sym)
	{
	void (*symbol)();

	/*
	 * Find the symbol. Try the fortran version first.
	 */
	if ((symbol = (void (*)())dso_sym (handle, fmangle(sym))) ||
	    (symbol = (void (*)())dso_sym (handle, sym)))
		return (void *)symbol;
	else
		return NULL;
	}

void *get_module_skeleton (char *module_name, char *sym)
	{
	module *mp;
	void *symbol = NULL;
	
	mp = find_module (module_name);
	if (mp)
		symbol = get_module_skeleton_internal (mp->handle, sym);
	else
		symbol = NULL;
	
	return symbol;
	}

void map_over_modules (char *sym, ...)
	{
	va_list arglist;
	void (*symbol)();
	module *mp;
	
	for (mp = modules; mp; mp = mp->next)
		{
		if (symbol = (void (*)())get_module_skeleton_internal (mp->handle, sym))
			{
			va_start (arglist, sym);
			(*symbol) (arglist);
			va_end (arglist);
			}
		}
	}

void *load_module (char *module_path, char *name)
	{
	dso_handle handle;
	module *mp;
	void (*symbol)();

	/*
	 * If the module is already loaded, just return its reference
	 */
	if (mp = find_module (name))
		return (mp->handle);

	/*
	 * else, prepare to load it by first getting a module structure
	 */
	if ((mp = malloc(sizeof (module))) == NULL)
		{
		last_error = "Unable to malloc module structure";
		return (NULL);
		}

	/*
	 * Actually try to load the module.
	 */
	handle = dso_load (module_path);
	
        if (handle == NULL)
                {
                last_error = dso_error ();
                free (mp);
                return (NULL);
                }

	/*
	 * It loaded! Fill in the module structure for this module.
	 */
	mp->name = name;
	mp->next = NULL;
	mp->last = last_module;
	mp->handle = handle;

	/*
	 * Thread the module structure onto the end of the list of all modules
	 */
	if (modules == NULL)
		modules = mp;
	if (last_module != NULL)
		last_module->next = mp;
	last_module = mp;

	/*
	 * Finally, invoke the init function, if any
	 */
	if (symbol = (void (*)())get_module_skeleton_internal (handle, "mcnpx_init_module"))
		{
		(*symbol)(main_argc, main_argv, main_envp);
		}

	return (handle);
	}
