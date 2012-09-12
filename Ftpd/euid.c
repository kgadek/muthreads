
/* based on otherlibs/unix/setuid.c,v 1.6 2005/03/25 23:56:37 kswadi Exp $ */

#include <mlvalues.h>
#include "unixsupport.h"

CAMLprim value unix_seteuid(value uid)
{
  if (seteuid(Int_val(uid)) == -1) uerror("seteuid", Nothing);
  return Val_unit;
}

CAMLprim value unix_setegid(value gid)
{
  if (setegid(Int_val(gid)) == -1) uerror("setegid", Nothing);
  return Val_unit;
}
