#include <dl-mod.h>
#include <fmangle.h>
#include <stdio.h>

void Fmangle(mcnp) ()
	{
	static void *(*sym)() = NULL;
	
	if (sym == NULL)
		{
		sym = get_module_skeleton ("mcnpx", "mcnp_skel");
		if (sym == NULL)
			{
			fprintf (stderr, "Can't locate module mcnpx/mcnp");
			exit (1);
			}
		}
	(void)(*sym)();
	}

