#include <clang-c/Index.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "completion.h"



/* Print "COMPLETION: " followed by the TypedText chunk of the completion
 * string to fp, that's the text that a user would be expected to type to get
 * this code-completion result. TypedText is the keyword for the client program
 * (emacs script in this case) to filter completion results.
 * 
 * This function returns the number of completion chunks on success, or it
 * would return an -1 if no TypedText chunk was found.
 */
static int completion_printCompletionHeadTerm(
    CXCompletionString completion_string, FILE *fp)
{
    int i_chunk  = 0;
    int n_chunks = clang_getNumCompletionChunks(completion_string);
    CXString ac_string;

    /* inspect all chunks only to find the TypedText chunk */
    for ( ; i_chunk < n_chunks; i_chunk++)
    {
        if (clang_getCompletionChunkKind(completion_string, i_chunk) 
            == CXCompletionChunk_TypedText)
        {
            /* We got it, just dump it to fp */
            ac_string = clang_getCompletionChunkText(completion_string, i_chunk);
            fprintf(fp, "COMPLETION: %s", clang_getCString(ac_string));
            clang_disposeString(ac_string);
            return n_chunks;    /* care package on the way */
        }
    }

    return -1;   /* We haven't found TypedText chunk in completion_string */
}


/* Print the completion line except the header term (COMPLETION: TypedText),
 * the output format should be identical with the result of clang -cc1
 * -code-completion-at. Here are some sample outputs from the clang code
 * completion process:

     COMPLETION: short
     COMPLETION: signed
     COMPLETION: static
     COMPLETION: Pattern : static_cast<<#type#>>(<#expression#>)
     COMPLETION: struct

 * However, here we don't handle Pattern explicitly because the emacs
 * script would simply drop those pattern lines with an regexp T T
 */
static void completion_printAllCompletionTerms(
    CXCompletionString completion_string, FILE *fp)
{
    int i_chunk  = 0;
    int n_chunks = clang_getNumCompletionChunks(completion_string);

    CXString chk_text;
    enum CXCompletionChunkKind chk_kind;

    for ( ; i_chunk < n_chunks; i_chunk++)
    {
        /* get the type and completion text of this chunk */
        chk_kind = clang_getCompletionChunkKind(completion_string, i_chunk);
        chk_text = clang_getCompletionChunkText(completion_string, i_chunk);
        
        /* differenct kinds of chunks has various output formats */
        switch (chk_kind)
        {
        case CXCompletionChunk_Placeholder:
            fprintf(fp, "<#%s#>", clang_getCString(chk_text));
            break;
                
        case CXCompletionChunk_ResultType:
            fprintf(fp, "[#%s#]", clang_getCString(chk_text));
            break;

        case CXCompletionChunk_Optional:
            /* print optional term in a recursive way */
            fprintf(fp, "{#");
            completion_printAllCompletionTerms(
                clang_getCompletionChunkCompletionString(completion_string, i_chunk),
                fp);
            fprintf(fp, "#}");
            break;
                
        default:
            fprintf(fp, "%s", clang_getCString(chk_text));
        }

        clang_disposeString(chk_text);
    }
}


/* Print specified completion string to fp. */
void completion_printCompletionLine(
    CXCompletionString completion_string, FILE *fp)
{
    /* print completion item head: COMPLETION: typed_string */
    if (completion_printCompletionHeadTerm(completion_string, fp) > 1)
    {
        /* If there's not only one TypedText chunk in this completion string,
         * we still have a lot of info to dump: 
         *
         *     COMPLETION: typed_text : ##infos## 
         */
        fprintf(fp, " : ");
        completion_printAllCompletionTerms(completion_string, fp);
    }

    printf("\n");
}

/* Print all completion results to fp */
void completion_printCodeCompletionResults(CXCodeCompleteResults *res, FILE *fp)
{
    unsigned int i = 0;
    for ( ; i < res->NumResults; i++) {
        completion_printCompletionLine(res->Results[i].CompletionString, fp);
    }
}
