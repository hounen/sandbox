 /*
  * Perform Fortran name mangling.
  * Currently, of the compilers we know, HP-UX leaves the name alone,
  * and everyone else adds a trailing underscore.
  *
  * StringifyFmangle is used to turn foo into "foo_"
  * Fmangle is used to turn foo into foo_
  * fmangle is used at runtime to produce a copy of "foo" with the mangling.
  *
  */
#if defined (HPUX)                                                              /*!fm         11*/
#if !defined (G77)                                                              /*!fm         12*/
#define StringifyFmangle(name) # name                                           /*!fm         13*/
#define Fmangle(name) name                                                      /*!fm         14*/
#define fmangle(name) name                                                      /*!fm         15*/
#else /* defined G77 */                                                         /*!fm         16*/
#define StringifyFmangle(name) # name "_"                                       /*!fm         17*/
#define Fmangle(name) name ## _                                                 /*!fm         18*/
#define fmangle(name) (strcat (strcpy (malloc (strlen(name) + 2), name), "_"))  /*!fm         19*/
#endif                                                                          /*!fm         20*/
#elif defined (AIX)                                                             /*!fm         21*/
#define StringifyFmangle(name) # name                                           /*!fm         22*/
#define Fmangle(name) name                                                      /*!fm         23*/
#define fmangle(name) name                                                      /*!fm         24*/
#else                                                                           /*!fm         25*/
#define StringifyFmangle(name) # name "_"                                       /*!fm         26*/
#define Fmangle(name) name ## _                                                 /*!fm         27*/
#define fmangle(name) (strcat (strcpy (malloc (strlen(name) + 2), name), "_"))  /*!fm         28*/
#endif                                                                          /*!fm         29*/
