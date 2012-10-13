#include <clang-c/Index.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <limits.h>

#include "completion.h"


/* Check if the first string is a prefix of the second string.
 * Case insensitive.
 */
static int isPrefix(char *prefix, char *string)
{
    char *prefix_pointer = prefix;
    char *string_pointer = string;
    while (*prefix_pointer != 0 && *string_pointer != 0) {
      if (toupper(*prefix_pointer) != toupper(*string_pointer)) {
            return 1;
        }
        prefix_pointer++;
        string_pointer++;
    }
    return 0;
}

/* Print "COMPLETION: " followed by the TypedText chunk of the completion
 * string to fp, that's the text that a user would be expected to type to get
 * this code-completion result. TypedText is the keyword for the client program
 * (emacs script in this case) to filter completion results.
 *
 * Only prints a completion if it matches the given prefix.
 * 
 * This function returns the number of matching completion chunks on success, or it
 * would return an -1 if no matching TypedText chunk was found.
 */
static int completion_printCompletionHeadTerm(
    CXCompletionString completion_string, FILE *fp, char *prefix)
{
    int i_chunk  = 0;
    int n_chunks = clang_getNumCompletionChunks(completion_string);
    CXString ac_string;

    /* inspect all chunks only to find the TypedText chunk */
    for ( ; i_chunk < n_chunks; i_chunk++) {
        if (clang_getCompletionChunkKind(completion_string, i_chunk) == CXCompletionChunk_TypedText) {
            /* We got it, dump it to fp if it matches the prefix */
            ac_string = clang_getCompletionChunkText(completion_string, i_chunk);
            char *cstring = (char *)clang_getCString(ac_string);
            if (isPrefix(prefix, cstring) > 0) {
              return -1;
            }
            fprintf(fp, "COMPLETION: %s", cstring);
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


/* Print specified completion string to fp.
 * Return 1 if a completion was printed. Otherwise, return 0.
 */
int completion_printCompletionLine(
    CXCompletionString completion_string, FILE *fp, char *prefix)
{
    /* print completion item head: COMPLETION: typed_string */
    int result = completion_printCompletionHeadTerm(completion_string, fp, prefix);
    if (result > 1) {
        /* If there's not only one TypedText chunk in this completion string,
         * we still have a lot of info to dump: 
         *
         *     COMPLETION: typed_text : ##infos## 
         */
        fprintf(fp, " : ");
        completion_printAllCompletionTerms(completion_string, fp);
        fprintf(fp, "\n");
        return 1;
    } else if (result > -1) {
        fprintf(fp, "\n");
        return 0;
    } else {
        return 0;
    }
}

/* Print the first MAX_COMPLETIONS_TO_PRINT completions to the fp. */
void completion_printCodeCompletionResults(CXCodeCompleteResults *res, FILE *fp, char *prefix)
{
    unsigned int i = 0;
    unsigned int n = 0;
    for ( ; i < res->NumResults; i++) {
        n += completion_printCompletionLine(res->Results[i].CompletionString, fp, prefix)
        if (n > MAX_COMPLETIONS_TO_PRINT) {
            break;
        }
    }
}
