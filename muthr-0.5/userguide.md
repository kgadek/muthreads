�threads for the user
=====================

The �threads are cooperative threads based on trampolined style (also known as continuation monad). See [lwc paper][1].

[1]: http://christophe.deleuze.free.fr/lwc/



Main primitives
---------------

  + `spawn`
  + `yield`
  + `sleep`
  + `nothing`
  + `start`
  + `stop`



MVars (and FIFOs ?)
-------------------

See [lwc paper][1].

**TODO**: create full mvar ?



I/O
---

  + `socket`
  + `connect`
  + `accept`
  + `read`
  + `write`
  + `close`
  + `recv_from`



I/O and MVars
-------------

Operations on I/O or MVars.  Operation blocks until either the MVar
can be taken, or the I/O operation can be performed.  If both are
possible immediately, the MVar is taken.

    read_or_take mv fd s ofs len kread ktake
    write_or_take mv fd s ofs len kwrite ktake
    accept_or_take



Exceptions
----------

Exceptions can be handled the usual way between cooperation points.
But the try/with construct cannot be used accross cooperation points.
A trywith function is provided for this purpose.

    trywith
      operation
      handler
      continuation

Operation is executed.  If it completes (after having possibly yield
or blocked any number of times) without any exception raised,
execution proceeds with continuation.

If an exception is raised during the execution of operation, it is
presented to handler.  This must be a function accepting a set of
exceptions, of the form:

    (function
      | Failure "aborted"
      | Sys.File_not_found ZZZ -> ... )

**THE HANDLER MUST NOT YIELD OR SUSPEND IN ANY WAY!!!**
If handler accepts the exception, it is executed before continuation.
Of course, trywiths can be nested, so if the handler does not accept
the exception, the handler of the enclosing trywith construct, if any,
is tried and so on.

If no handler catches the exception the thread is killed and a warning
is printed on stderr. ZZZ

If some result value is to be passed from operation to continuation,
the following variant should be used:

    trywithk
      operation
      handler
      continuation

where operation takes its continuation as argument and passes it its
result.


### Example

    trywithk
      (fun k () ->
        resolv1 [] qn qt qc >>= fun cnames res ->
        k (cnames, res))
      (fun e ->
        let m = make_empty_answer m 2 in
        dbg_msg ">>>> sending reply" m;
        dns_send s m;
        print_string "resolv failed"; raise e)
      (gotit handle)


Note how operation applies its result on k.  It will be passed to
(gotit handle) (which thus must be a fun accepting the (cnames, res)
pair as argument.

Common error: operation does not use its k.  If it does not raise any
exception either, an no match exception will be raised when trywithk
incorrectly calls the continuation (see code for details).



Timeouts
--------

    timeout
        t              -- a float value representing a time in seconds
        operation
        when_expired
        when_normal

The thread starts executing 'operation' (of course it may block, or
suspend or whatever).  If operation terminates in the specified delay,
execution proceeds with 'when_normal', otherwise it proceeds with
'when_expired'.  Timeouts can be nested.

**WARNING** : if when_normal is set to nothing, context accumulate ??? ZZZ

If a result is to be passed from operation to when_normal, the
alternate form should be used

    timeoutk
        t
        operation
        when_expired
        when_normal

where operation takes its continuation (if it completes before the
timer it will be given when_normal) as parameter, and so can pass it
its own result.

### Example:

    ZZZ



Retry
-----

Retrying an operation if it fails is a common need in some
applications.  For example, a TFTP client or server needs to send a
message and wait for an ACK.  It should resend it if the ACK doesn't
show up before a timer expires.

...



`gethostbyname`
-------------

`gethostbyname` is the classical (although now considered obsolete)
function to query a local DNS resolver.

The Unix module provides it but as a simple wrapper to the
corresponding C library function, which is blocking.

    val gethostbyname : string -> host_entry
    (** Find an entry in [hosts] with the given name, or raise
        [Not_found]. *)

�threads provide a non blocking gethostbyname that, as a blocking
operation, takes its continuation as argument.  

    val gethostbyname : string -> (Unix.host_entry -> unit) -> unit

Note that this function only queries the DNS resolvers found in
`/etc/resolv.conf` .  The C function (and so the OCaml Unix module
binding) may also query the /etc/hosts file or the NIS (or YP)
service.


**TODO**: getaddrinfo



Graphics
--------

