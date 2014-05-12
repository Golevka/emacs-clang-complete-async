#ifndef _COMPLETION_SESSION_H_
#define _COMPLETION_SESSION_H_


#include <clang-c/Index.h>
#include <stdio.h>


typedef struct __completion_Session_struct
{
    /* <source file properties> */
    const char *src_filename;  /* filename of the source file */
    char *src_buffer;          /* buffer holding the source code */
    int   src_length;          /* length of the source code <including the trailing '\0'> */
    int   buffer_capacity;     /* size of source code buffer */

    /* <clang args properties> */
    int  num_args;         /* number of command line arguments */
    char **cmdline_args;   /* command line arguments to pass to the clang
                            * driver */

    /* <clang parser objects> */
    CXIndex           cx_index;
    CXTranslationUnit cx_tu;

    /* <clang parse options> */
    unsigned  ParseOptions;
    unsigned  CompleteAtOptions;

} completion_Session;


/* COMPLETION SERVER DEFAULT SETTINGS */

#define  DEFAULT_PARSE_OPTIONS       CXTranslationUnit_PrecompiledPreamble
#define  DEFAULT_COMPLETEAT_OPTIONS  CXCodeComplete_IncludeMacros
#define  INITIAL_SRC_BUFFER_SIZE     4096    /* 4KB */



/* clang_defaultEditingTranslationUnitOptions() */
/* CXTranslationUnit_PrecompiledPreamble */




/* Initialize basic information for completion, such as source filename, initial source 
   buffer and command line arguments to pass to clang */
void 
__initialize_completionSession(int argc, char *argv[], completion_Session *session);

/* Initialize session object and launch the completion server, preparse the source file and 
   build the AST for furture code completion requests  */
void startup_completionSession(int argc, char *argv[], completion_Session *session);


/* Print specified completion string to fp. */
void completion_printCompletionLine(CXCompletionString completion_string, FILE *fp);

/* Print all completion results to fp */
void completion_printCodeCompletionResults(CXCodeCompleteResults *res, FILE *fp);


/* Simple wrappers for clang parser functions */

CXTranslationUnit completion_parseTranslationUnit(completion_Session *session);
int completion_reparseTranslationUnit(completion_Session *session);
CXCodeCompleteResults* completion_codeCompleteAt(
    completion_Session *session, int line, int column);



#endif /* _COMPLETION_SESSION_H_ */
