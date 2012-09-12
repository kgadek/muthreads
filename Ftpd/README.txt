#                                                                    -*- mode: org -*-
	       muftpd, an ftp server based on muthreads
	       ----------------------------------------


* Installation

Needs to have the muthreads library installed.

PAM uses the configuration file /etc/pam.conf or the files in
/etc/pam.d/ if this directory exists, which is generally the case.

Each file in the directory has the name of a 'service'.  The file
describes the authentication tasks required by the service.

muftpd uses the "muftpd" service, so a file /etc/pam.d/muftpd must be
created.  The main point is we don't want PAM to add any delay on
authentication failure: this would block the whole process and so
all our threads! We'll add the delay ourselves.

The content of this file can be as simple as the following line:

auth required pam_unix.so nodelay

It says we want authentication through the standard unix module, and
tells it not to add any delay on failure.

TODO: see pam_ftp for anonymous access

* Running

Must be root (in order for the seteuid calls to be possible).

 # ocamlrun -I . ftpd
 # ./ftpd.opt

* Files

README.txt  this file

euid.c      wrappers for providing access to Linux syscalls
            seteuid/setgeuid

euid.mli
euid.ml     bindings for the above

data.mli    data type definitions
	    FTP type, mode, struct, commands

parse.mli
parse.ml    parsing functions for FTP commands (and their arguments)
            + make_pasv_text

aux.mli
aux.ml      auxiliary functions
            CR/LF conversion, split a string into lines, create unique
            file name, interface to PAM, set uid/euid, various utility
            functions...

ftpd.ml     main file
	    code for PI and DTP threads

muftpd      service file for PAM, to be copied in /etc/pam.d/

* Dynamic structure

The server is made of (only) three kinds of thread:

- main
  wait for connection requests, accept and spawn a pi thread to
  handle it.

- pi (protocol interpreter, RFC959 lingo)
  The FTP session controler.  Receive commands, send back replies,
  spawn dtp thread.

  Each PI thread handles local state:
   - user id
   - current directory
   - transfer type
   - statistics

   The 'pi' function is made of a series of mutually recursive
   functions representing the different situations of the PI.

- dtp (data transfer process, RFC959 lingo)
  One such thread is spawned by a PI each time a data transfer is to
  happen.  

  There are two kinds of DTP threads: dtp_passive and dtp_active.

Related pi and dtp threads synchronize and communicate exclusively
through mvars.

Since each pi thread has its user_id and current_directory, it has to
enforce them when resumed, by calling set_euid and Unix.chdir.  It
actually does it only when about to access the filesystem.  Current
directory and access rights are related to file names and are not
relevant once we operate on file descriptors.  Thus, we don't have to
switch uids and dirs during a data transfer.

There's always one 'main' thread running, one 'pi' thread for each
session, and one 'dtp' thread for each current data transfer (there
can be no more than one data transfer for each pi at any given time).

For more details, the code is written in literate style.

* Notes

check the telnet Synch (IAC IP IAC)
try with various ftp clients

take_or_recv : replace with take_or_read ?
