#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <fmangle.h>

extern char *last_error;

typedef struct linebuf
	{
	struct linebuf *next;
	char *module;
	char *alias;
	} linebuf;

/*
 * A module configuration file is of the following form:
 * Any line starting with a '#' is a comment.
 * other lines are of the form:
 *   module_alias module_path
 * So a line loading my_module.so with a module name of my_stuff would be
 *   my_stuff /usr/local/lib/my_module.so
 * Future uses of find_module and get_module_skeleton would use the name
 * my_stuff.
 */

#define BUFSIZE 1024
linebuf *read_configuration (FILE *fp)
	{
	char buf[BUFSIZE];
	linebuf *lbuf = NULL;
	linebuf *current;
	linebuf *first = NULL;
	int line = 0;
	char *alias;
	char *path;
	int alias_length;
	int path_length;

	while (fgets (buf, BUFSIZE, fp))
		{
		if (buf[0] == '#')
			continue;
		current = lbuf;
		alias = strtok (buf, " \t");
		path = strtok (NULL, " \t\n");
		if (alias == NULL || path == NULL)
			{
			last_error = "buf";
			do_error ("Bad module configuration file format.");
			}
		alias_length = strlen(alias) + 1;
		path_length = strlen (path) + 1;
		
		if ((lbuf = (linebuf *)malloc (sizeof (linebuf *))) == NULL) 
			{
			last_error = "Malloc failed";
			do_error ("Can't store configuration information.");
			}
		if ((lbuf->alias = malloc (alias_length)) == NULL)
			{
			last_error = "Malloc failed";
			do_error ("Can't store module alias.");
			}
		strcpy (lbuf->alias, alias);
		if ((lbuf->module = malloc (path_length)) == NULL)
			{
			last_error = "Malloc failed";
			do_error ("Can't store module path.");
			}
		strcpy (lbuf->module, path);

		if (first == NULL)
			first = lbuf;
		if (current)
			current->next = lbuf;
		lbuf->next = NULL;
		}
	return first;
	}
	
int main(int argc, char *argv[], char *env[])
	{
	FILE *fp;
	static char buf[512];
	void *handle;
	void (*f)();
	linebuf *lp;
	extern init_module_system (int, char **, char **);

	if (argc <= 1)
		{
		sprintf (buf, "%s modules program-args ...", argv[0]);
		last_error = buf;
		do_error ("Usage");
		}

	init_module_system (argc, argv, env);
	
	if ((fp = fopen (argv[1], "r")) == NULL)
		{
		last_error = argv[1];
		do_error ("Open");
		}
	
	lp = read_configuration (fp);
	while (lp)
		{
		if ((handle = load_module (lp->module, lp->alias)) == NULL)
			do_error (lp->module);
		lp = lp->next;
		}
	map_over_modules ("mcnpx_finish_module");
	exit (0);
	}
