#include <stdlib.h>
#include <string.h>
#include "completion.h"



/* Copy command line parameters (except source filename) to cmdline_args  */
static void __copy_cmdlineArgs(int argc, char *argv[], completion_Session *session)
{
    int i_arg = 0;
    session->num_args = argc - 2;  /* argv[0] and argv[argc - 2] should be discarded */
    session->cmdline_args = (char**)calloc(sizeof(char*), session->num_args);

    /* copy argv[1..argc-1] to cmdline_args */
    for ( ; i_arg < session->num_args; i_arg++)
    {
        session->cmdline_args[i_arg] = 
            (char*)calloc(sizeof(char), strlen(argv[i_arg + 1]) + 1);

        strcpy(session->cmdline_args[i_arg], argv[i_arg + 1]);
    }
}

/* Initialize basic information for completion, such as source filename, initial source 
   buffer and command line arguments for clang */
void 
__initialize_completionSession(int argc, char *argv[], completion_Session *session)
{
    /* filename shall be the last parameter */
    session->src_filename = argv[argc - 1];
    session->src_length = 0;      /* we haven't read any source code yet. */
    session->buffer_capacity = INITIAL_SRC_BUFFER_SIZE;
    session->src_buffer = (char*)calloc(sizeof(char), session->buffer_capacity);

    __copy_cmdlineArgs(argc, argv, session);
}


/* Initialize session object and launch the completion server, preparse the source file and 
   build the AST for furture code completion requests  
*/
void startup_completionSession(int argc, char *argv[], completion_Session *session)
{
    __initialize_completionSession(argc, argv, session);

    /* default parameters */
    session->ParseOptions      = DEFAULT_PARSE_OPTIONS;
    session->CompleteAtOptions = DEFAULT_COMPLETEAT_OPTIONS;

    session->cx_index = clang_createIndex(0, 0);
    completion_parseTranslationUnit(session);
    completion_reparseTranslationUnit(session);
}


/* Simple wrappers for clang parser functions */

static struct CXUnsavedFile __get_CXUnsavedFile(const completion_Session *session)
{
    struct CXUnsavedFile unsaved_files;
    unsaved_files.Filename = session->src_filename;
    unsaved_files.Contents = session->src_buffer;
    unsaved_files.Length   = session->src_length;

    return unsaved_files;
}

CXTranslationUnit 
completion_parseTranslationUnit(completion_Session *session)
{
    struct CXUnsavedFile unsaved_files = __get_CXUnsavedFile(session);
    session->cx_tu = 
        clang_parseTranslationUnit(
            session->cx_index, session->src_filename, 
            (const char * const *) session->cmdline_args, session->num_args, 
            &unsaved_files, 1,
            session->ParseOptions);

    return session->cx_tu;
}

int completion_reparseTranslationUnit(completion_Session *session)
{
    struct CXUnsavedFile unsaved_files = __get_CXUnsavedFile(session);
    return 
        clang_reparseTranslationUnit(
            session->cx_tu, 1, &unsaved_files, session->ParseOptions);
}

CXCodeCompleteResults* 
completion_codeCompleteAt(
    completion_Session *session, int line, int column)
{
    struct CXUnsavedFile unsaved_files = __get_CXUnsavedFile(session);
    return 
        clang_codeCompleteAt(
            session->cx_tu, session->src_filename, line, column, 
            &unsaved_files, 1, session->CompleteAtOptions);
}
