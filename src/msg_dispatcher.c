#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include "msg_callback.h"


/* command - message tuple for commander pattern */
struct __command_dispatch_entry {
    const char *command;
    void (*command_handler)(completion_Session*, FILE*);
};

/* message dispatch table */
struct __command_dispatch_entry 
__command_dispatch_table[] = 
{
    {"COMPLETION",   completion_doCompletion},
    {"SOURCEFILE",   completion_doSourcefile},
    {"CMDLINEARGS",  completion_doCmdlineArgs},
    {"SYNTAXCHECK",  completion_doSyntaxCheck},
    {"REPARSE",      completion_doReparse},
    {"SHUTDOWN",     completion_doShutdown}
};


/* Dispatch different messages to their corresponding message handlers */
void completion_AcceptRequest(completion_Session *session, FILE *fp)
{
    unsigned int i_entry = 0;
    char msg_head[LINE_MAX];
    fscanf(fp, "%s", msg_head);

    /* find corresponded message handler to dispatch message to */
    for ( ; i_entry < 
              sizeof(__command_dispatch_table)/
              sizeof(__command_dispatch_table[0]); i_entry++)
    {
        if (strcmp(msg_head, 
                __command_dispatch_table[i_entry].command) == 0)
        {
            fgets(msg_head, sizeof(msg_head), fp); /* skip trailing '\n' */
            __command_dispatch_table[i_entry].command_handler(session, fp);
            return;
        }
    }

    /* no message handler was found */
    printf("ERROR: UNKNOWN COMMAND: %s", msg_head); fflush(stdout);
}
