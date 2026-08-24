#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>

#ifdef G77
#include <g2c.h>
#endif

#ifdef PGF77
int __argc_save;
char **__argv_save;
#endif

#include <fmangle.h>

static int saved_argc;
static char **saved_argv;
static char **saved_env;

#if defined (GENERIC_ENV)
/*
 * Local versions of getenv, iargc and getarg.
 *
 * The reason for placing these here is that, although getarg, iargc, and
 * getenv are in everyone's f77 libraries, NOT everyone tells you how to
 * set them up.
 * Most every f77 vendor provides a chapter somewhere on "FORTRAN C
 * Interoprability". This allways covers data (How is an integer pased
 * between f77 and C functions), and sometimes addresses how to set
 * things up so that a C main program can call FORTRAN subroutines as
 * we do here. The problem is that the initialization routine is usually
 * just to initialize the I/O system. I have not yet found a case
 * where information is provided on how to get C's ac, av, and environement
 * down to the f77 libraries so that getarg, iargc, and getenv can work.
 *
 * With g77, I can look at the source. Thus the #ifdef G77.
 * With Sun, I filed a bug report (service order number 3615702, bugid
 *       4182125, for those interested) that resulted in the information
 *       below for #ifdef SUN. This is only for Sun's Version 5. For the
 *       new version 6 stuff, they changed the unpublished interface.
 * At that point, the thought of dealing with HP, SGI, IBM, et.al. was too
 * overwhelming and I just wrote these. The saving grace is that almost
 * every f77 implements passing character strings as a pointer to the
 * string, and an additional parameter for the length.
 *
 * call getarg (position, value)
 *     position: integer scaler intent(in)
 *     value: character scaler intent(out)
 * i = iargc ()
 *     iargc: integer function
 * call getenv (name, value)
 *     name: character scaler intent(in)
 *     value: character scaler intent(out)
 */

void Fmangle(generic_getarg) (int *position, char *value, int valuelen)
	{
	char *temp;
	int i;

	/*
	 * point temp at the argument checking its range
	 */
	if ((0 <= *position) && (*position < saved_argc))
		temp = saved_argv[*position];
	else
		temp = "";
	/*
	 * copy the argument string being careful about the
	 * target length
	 */
	for (i = 0; i < valuelen; i++)
		if (*temp == '\0')
			break;
		else
			*value++ = *temp++;
	/*
	 * pad the target with blanks
	 */
	while (i < valuelen)
		{
		*value++ = ' ';
		i++;
		}
	}

void Fmangle(generic_iargc) (int *argc)
	{
	*argc = saved_argc - 1;
	}

void Fmangle(generic_getenv) (char *name, char *value, int namelen, int valuelen)
	{
	char stringbuf[128];
	int i;
	char *env;

	
	/*
	 * If an input name was provided...
	 */
	if (namelen > 0)
		{
		/*
		 * then copy the input name into a C string
		 * and call getenv on it.
		 * The copy loop stops if
		 *   1. the name overflows our internal buffer,
		 *   2. we reach the end of the stated length of the name,
		 *   3. The name string has a space.
		 */

		for (i = 0; i < 128 || i == namelen || name[i] == ' '; i++)
			stringbuf[i] = name[i];

		/*
		 * We now have some sort of name in our stringbuf.
		 * C-terminate it and see if it is in the environment.
		 */

		stringbuf[i] = 0;
		env = getenv (stringbuf);

		/*
		 * With the result of getenv in env, copy into
		 * the fortran return value.
		 */

		if (env)
			{
			while (*env && (valuelen-- > 0))
				*value++ = *env++;
			}
		}
	
	/*
	 * Pad value[] with trailing blanks. Note we fall through
	 * to here if no name is provided and an empty string returns.
	 */
	while (valuelen-- > 0)
		*value++ = ' ';
	return;
	}
#endif /* GENERIC_ENV */

void mcnpx_init_module (int argc, char **argv, char **env)
	{
	/*
	 * declerations
	 */
#ifdef G77
	extern void f_setarg (int, char **);
	extern void f_setsig (void);
	extern void f_init (void);
#endif

	/*
	 * Real code as needed.
	 * We save ac, av, and env regardless of architecture.
	 */

	saved_argc = argc - 1;
	saved_argv = argv + 1;
	saved_env = env;

#ifdef G77
	(void)f_setarg (saved_argc, saved_argv);
	(void)f_setsig ();
	(void)f_init ();
#endif
#ifdef PGF77
        __argc_save = saved_argc;
        __argv_save = saved_argv;
#endif
	}

void mcnpx_finish_module ()
	{
#ifdef G77
	extern void f_exit(void);
	
	(void)f_exit();
#endif
	}
