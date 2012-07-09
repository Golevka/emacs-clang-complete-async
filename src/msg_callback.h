#ifndef _COMPLETION_PROTOCOL_H_
#define _COMPLETION_PROTOCOL_H_



#include "completion.h"


/* Dispatch different messages to their corresponding message handlers */
void completion_AcceptRequest(completion_Session *session, FILE *fp);


/* 
   MESSAGE HANDLERS: callback to handle recieved requests

   COMPLETION: Do code completion at a specified point.
   Message format: 
        row:[#row#]
        column:[#column#]
        source_length:[#src_length#]
        <# SOURCE CODE #>

   SOURCEFILE: Update the source code in the source buffer (session->src_buffer)
   Message format:
        source_length:[#src_length#]
        <# SOURCE CODE #>

   CMDLINEARGS: Specify command line arguments passing to clang parser.
   Message format:
        num_args:[#n_args#]
        arg1 arg2 ...... (there should be n_args items here)

   REPARSE: Reparse the source code
   [no message body]

   SYNTAXCHECK: Retrieve diagnostic messages
   Message format:
        source_length:[#src_length#]
        <# SOURCE CODE #>

   SHUTDOWN: Shut down the completion server (this program)
   [no message body]
*/

/* message handlers */
void completion_doCompletion(completion_Session *session, FILE *fp);   /* COMPLETION */
void completion_doSourcefile(completion_Session *session, FILE *fp);   /* SOURCEFILE */
void completion_doCmdlineArgs(completion_Session *session, FILE *fp);  /* CMDLINEARGS */
void completion_doReparse(completion_Session *session, FILE *fp);      /* REPARSE */
void completion_doSyntaxCheck(completion_Session *session, FILE *fp);  /* SYNTAXCHECK */
void completion_doShutdown(completion_Session *session, FILE *fp);     /* SHUTDOWN */


#endif /* _COMPLETION_PROTOCOL_H_ */
