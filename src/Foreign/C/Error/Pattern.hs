{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}

module Foreign.C.Error.Pattern
  ( pattern EOK
  , pattern E2BIG
  , pattern EACCES
  , pattern EADDRINUSE
  , pattern EADDRNOTAVAIL
  , pattern EADV
  , pattern EAFNOSUPPORT
  , pattern EAGAIN
  , pattern EALREADY
  , pattern EBADF
  , pattern EBADMSG
  , pattern EBADRPC
  , pattern EBUSY
  , pattern ECHILD
  , pattern ECOMM
  , pattern ECONNABORTED
  , pattern ECONNREFUSED
  , pattern ECONNRESET
  , pattern EDEADLK
  , pattern EDESTADDRREQ
  , pattern EDIRTY
  , pattern EDOM
  , pattern EDQUOT
  , pattern EEXIST
  , pattern EFAULT
  , pattern EFBIG
  , pattern EFTYPE
  , pattern EHOSTDOWN
  , pattern EHOSTUNREACH
  , pattern EIDRM
  , pattern EILSEQ
  , pattern EINPROGRESS
  , pattern EINTR
  , pattern EINVAL
  , pattern EIO
  , pattern EISCONN
  , pattern EISDIR
  , pattern ELOOP
  , pattern EMFILE
  , pattern EMLINK
  , pattern EMSGSIZE
  , pattern EMULTIHOP
  , pattern ENAMETOOLONG
  , pattern ENETDOWN
  , pattern ENETRESET
  , pattern ENETUNREACH
  , pattern ENFILE
  , pattern ENOBUFS
  , pattern ENODATA
  , pattern ENODEV
  , pattern ENOENT
  , pattern ENOEXEC
  , pattern ENOLCK
  , pattern ENOLINK
  , pattern ENOMEM
  , pattern ENOMSG
  , pattern ENONET
  , pattern ENOPROTOOPT
  , pattern ENOSPC
  , pattern ENOSR
  , pattern ENOSTR
  , pattern ENOSYS
  , pattern ENOTBLK
  , pattern ENOTCONN
  , pattern ENOTDIR
  , pattern ENOTEMPTY
  , pattern ENOTSOCK
  , pattern ENOTSUP
  , pattern ENOTTY
  , pattern ENXIO
  , pattern EOPNOTSUPP
  , pattern EPERM
  , pattern EPFNOSUPPORT
  , pattern EPIPE
  , pattern EPROCLIM
  , pattern EPROCUNAVAIL
  , pattern EPROGMISMATCH
  , pattern EPROGUNAVAIL
  , pattern EPROTO
  , pattern EPROTONOSUPPORT
  , pattern EPROTOTYPE
  , pattern ERANGE
  , pattern EREMCHG
  , pattern EREMOTE
  , pattern EROFS
  , pattern ERPCMISMATCH
  , pattern ERREMOTE
  , pattern ESHUTDOWN
  , pattern ESOCKTNOSUPPORT
  , pattern ESPIPE
  , pattern ESRCH
  , pattern ESRMNT
  , pattern ESTALE
  , pattern ETIME
  , pattern ETIMEDOUT
  , pattern ETOOMANYREFS
  , pattern ETXTBSY
  , pattern EUSERS
  , pattern EWOULDBLOCK
  , pattern EXDEV
  ) where

import Foreign.C.Error (Errno(Errno))

#include "HsBaseConfig.h"

pattern EOK :: Errno
pattern EOK = Errno 0
pattern E2BIG :: Errno
pattern E2BIG = Errno (CONST_E2BIG)
pattern EACCES :: Errno
pattern EACCES = Errno (CONST_EACCES)
pattern EADDRINUSE :: Errno
pattern EADDRINUSE = Errno (CONST_EADDRINUSE)
pattern EADDRNOTAVAIL :: Errno
pattern EADDRNOTAVAIL = Errno (CONST_EADDRNOTAVAIL)
pattern EADV :: Errno
pattern EADV = Errno (CONST_EADV)
pattern EAFNOSUPPORT :: Errno
pattern EAFNOSUPPORT = Errno (CONST_EAFNOSUPPORT)
pattern EAGAIN :: Errno
pattern EAGAIN = Errno (CONST_EAGAIN)
pattern EALREADY :: Errno
pattern EALREADY = Errno (CONST_EALREADY)
pattern EBADF :: Errno
pattern EBADF = Errno (CONST_EBADF)
pattern EBADMSG :: Errno
pattern EBADMSG = Errno (CONST_EBADMSG)
pattern EBADRPC :: Errno
pattern EBADRPC = Errno (CONST_EBADRPC)
pattern EBUSY :: Errno
pattern EBUSY = Errno (CONST_EBUSY)
pattern ECHILD :: Errno
pattern ECHILD = Errno (CONST_ECHILD)
pattern ECOMM :: Errno
pattern ECOMM = Errno (CONST_ECOMM)
pattern ECONNABORTED :: Errno
pattern ECONNABORTED = Errno (CONST_ECONNABORTED)
pattern ECONNREFUSED :: Errno
pattern ECONNREFUSED = Errno (CONST_ECONNREFUSED)
pattern ECONNRESET :: Errno
pattern ECONNRESET = Errno (CONST_ECONNRESET)
pattern EDEADLK :: Errno
pattern EDEADLK = Errno (CONST_EDEADLK)
pattern EDESTADDRREQ :: Errno
pattern EDESTADDRREQ = Errno (CONST_EDESTADDRREQ)
pattern EDIRTY :: Errno
pattern EDIRTY = Errno (CONST_EDIRTY)
pattern EDOM :: Errno
pattern EDOM = Errno (CONST_EDOM)
pattern EDQUOT :: Errno
pattern EDQUOT = Errno (CONST_EDQUOT)
pattern EEXIST :: Errno
pattern EEXIST = Errno (CONST_EEXIST)
pattern EFAULT :: Errno
pattern EFAULT = Errno (CONST_EFAULT)
pattern EFBIG :: Errno
pattern EFBIG = Errno (CONST_EFBIG)
pattern EFTYPE :: Errno
pattern EFTYPE = Errno (CONST_EFTYPE)
pattern EHOSTDOWN :: Errno
pattern EHOSTDOWN = Errno (CONST_EHOSTDOWN)
pattern EHOSTUNREACH :: Errno
pattern EHOSTUNREACH = Errno (CONST_EHOSTUNREACH)
pattern EIDRM :: Errno
pattern EIDRM = Errno (CONST_EIDRM)
pattern EILSEQ :: Errno
pattern EILSEQ = Errno (CONST_EILSEQ)
pattern EINPROGRESS :: Errno
pattern EINPROGRESS = Errno (CONST_EINPROGRESS)
pattern EINTR :: Errno
pattern EINTR = Errno (CONST_EINTR)
pattern EINVAL :: Errno
pattern EINVAL = Errno (CONST_EINVAL)
pattern EIO :: Errno
pattern EIO = Errno (CONST_EIO)
pattern EISCONN :: Errno
pattern EISCONN = Errno (CONST_EISCONN)
pattern EISDIR :: Errno
pattern EISDIR = Errno (CONST_EISDIR)
pattern ELOOP :: Errno
pattern ELOOP = Errno (CONST_ELOOP)
pattern EMFILE :: Errno
pattern EMFILE = Errno (CONST_EMFILE)
pattern EMLINK :: Errno
pattern EMLINK = Errno (CONST_EMLINK)
pattern EMSGSIZE :: Errno
pattern EMSGSIZE = Errno (CONST_EMSGSIZE)
pattern EMULTIHOP :: Errno
pattern EMULTIHOP = Errno (CONST_EMULTIHOP)
pattern ENAMETOOLONG :: Errno
pattern ENAMETOOLONG = Errno (CONST_ENAMETOOLONG)
pattern ENETDOWN :: Errno
pattern ENETDOWN = Errno (CONST_ENETDOWN)
pattern ENETRESET :: Errno
pattern ENETRESET = Errno (CONST_ENETRESET)
pattern ENETUNREACH :: Errno
pattern ENETUNREACH = Errno (CONST_ENETUNREACH)
pattern ENFILE :: Errno
pattern ENFILE = Errno (CONST_ENFILE)
pattern ENOBUFS :: Errno
pattern ENOBUFS = Errno (CONST_ENOBUFS)
pattern ENODATA :: Errno
pattern ENODATA = Errno (CONST_ENODATA)
pattern ENODEV :: Errno
pattern ENODEV = Errno (CONST_ENODEV)
pattern ENOENT :: Errno
pattern ENOENT = Errno (CONST_ENOENT)
pattern ENOEXEC :: Errno
pattern ENOEXEC = Errno (CONST_ENOEXEC)
pattern ENOLCK :: Errno
pattern ENOLCK = Errno (CONST_ENOLCK)
pattern ENOLINK :: Errno
pattern ENOLINK = Errno (CONST_ENOLINK)
pattern ENOMEM :: Errno
pattern ENOMEM = Errno (CONST_ENOMEM)
pattern ENOMSG :: Errno
pattern ENOMSG = Errno (CONST_ENOMSG)
pattern ENONET :: Errno
pattern ENONET = Errno (CONST_ENONET)
pattern ENOPROTOOPT :: Errno
pattern ENOPROTOOPT = Errno (CONST_ENOPROTOOPT)
pattern ENOSPC :: Errno
pattern ENOSPC = Errno (CONST_ENOSPC)
pattern ENOSR :: Errno
pattern ENOSR = Errno (CONST_ENOSR)
pattern ENOSTR :: Errno
pattern ENOSTR = Errno (CONST_ENOSTR)
pattern ENOSYS :: Errno
pattern ENOSYS = Errno (CONST_ENOSYS)
pattern ENOTBLK :: Errno
pattern ENOTBLK = Errno (CONST_ENOTBLK)
pattern ENOTCONN :: Errno
pattern ENOTCONN = Errno (CONST_ENOTCONN)
pattern ENOTDIR :: Errno
pattern ENOTDIR = Errno (CONST_ENOTDIR)
pattern ENOTEMPTY :: Errno
pattern ENOTEMPTY = Errno (CONST_ENOTEMPTY)
pattern ENOTSOCK :: Errno
pattern ENOTSOCK = Errno (CONST_ENOTSOCK)
pattern ENOTSUP :: Errno
pattern ENOTSUP = Errno (CONST_ENOTSUP)
pattern ENOTTY :: Errno
pattern ENOTTY = Errno (CONST_ENOTTY)
pattern ENXIO :: Errno
pattern ENXIO = Errno (CONST_ENXIO)
pattern EOPNOTSUPP :: Errno
pattern EOPNOTSUPP = Errno (CONST_EOPNOTSUPP)
pattern EPERM :: Errno
pattern EPERM = Errno (CONST_EPERM)
pattern EPFNOSUPPORT :: Errno
pattern EPFNOSUPPORT = Errno (CONST_EPFNOSUPPORT)
pattern EPIPE :: Errno
pattern EPIPE = Errno (CONST_EPIPE)
pattern EPROCLIM :: Errno
pattern EPROCLIM = Errno (CONST_EPROCLIM)
pattern EPROCUNAVAIL :: Errno
pattern EPROCUNAVAIL = Errno (CONST_EPROCUNAVAIL)
pattern EPROGMISMATCH :: Errno
pattern EPROGMISMATCH = Errno (CONST_EPROGMISMATCH)
pattern EPROGUNAVAIL :: Errno
pattern EPROGUNAVAIL = Errno (CONST_EPROGUNAVAIL)
pattern EPROTO :: Errno
pattern EPROTO = Errno (CONST_EPROTO)
pattern EPROTONOSUPPORT :: Errno
pattern EPROTONOSUPPORT=Errno (CONST_EPROTONOSUPPORT)
pattern EPROTOTYPE :: Errno
pattern EPROTOTYPE = Errno (CONST_EPROTOTYPE)
pattern ERANGE :: Errno
pattern ERANGE = Errno (CONST_ERANGE)
pattern EREMCHG :: Errno
pattern EREMCHG = Errno (CONST_EREMCHG)
pattern EREMOTE :: Errno
pattern EREMOTE = Errno (CONST_EREMOTE)
pattern EROFS :: Errno
pattern EROFS = Errno (CONST_EROFS)
pattern ERPCMISMATCH :: Errno
pattern ERPCMISMATCH = Errno (CONST_ERPCMISMATCH)
pattern ERREMOTE :: Errno
pattern ERREMOTE = Errno (CONST_ERREMOTE)
pattern ESHUTDOWN :: Errno
pattern ESHUTDOWN = Errno (CONST_ESHUTDOWN)
pattern ESOCKTNOSUPPORT :: Errno
pattern ESOCKTNOSUPPORT=Errno (CONST_ESOCKTNOSUPPORT)
pattern ESPIPE :: Errno
pattern ESPIPE = Errno (CONST_ESPIPE)
pattern ESRCH :: Errno
pattern ESRCH = Errno (CONST_ESRCH)
pattern ESRMNT :: Errno
pattern ESRMNT = Errno (CONST_ESRMNT)
pattern ESTALE :: Errno
pattern ESTALE = Errno (CONST_ESTALE)
pattern ETIME :: Errno
pattern ETIME = Errno (CONST_ETIME)
pattern ETIMEDOUT :: Errno
pattern ETIMEDOUT = Errno (CONST_ETIMEDOUT)
pattern ETOOMANYREFS :: Errno
pattern ETOOMANYREFS = Errno (CONST_ETOOMANYREFS)
pattern ETXTBSY :: Errno
pattern ETXTBSY = Errno (CONST_ETXTBSY)
pattern EUSERS :: Errno
pattern EUSERS = Errno (CONST_EUSERS)
pattern EWOULDBLOCK :: Errno
pattern EWOULDBLOCK = Errno (CONST_EWOULDBLOCK)
pattern EXDEV :: Errno
pattern EXDEV = Errno (CONST_EXDEV)
