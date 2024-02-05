module Foreign.C.Error.Describe
  ( -- * Error Enum Names
    enumString
  , enumByteArray
  , enumShortText
  , enumText

    -- * Error Descriptions
  , descriptionString
  , descriptionByteArray
  , descriptionShortText
  , descriptionText

    -- * Legacy Function Names
  , string
  , byteArray
  ) where

import Control.Monad (when)
import Control.Monad.ST (runST)
import Data.ByteString.Short.Internal (ShortByteString (SBS))
import Data.Char (chr, ord)
import Data.Foldable (for_)
import Data.Primitive (ByteArray (..), SmallArray)
import Data.Primitive.Unlifted.Array (UnliftedArray)
import Data.Text (Text)
import Data.Text.Short (ShortText)
import Data.Word (Word8)
import Foreign.C.Error (Errno (..))
import Foreign.C.Types (CInt)

import qualified Data.Primitive as PM
import qualified Data.Primitive.Unlifted.Array as PM
import qualified Data.Text.Short as TS
import qualified Data.Text.Short.Unsafe as TS
import qualified GHC.Exts as E

string :: Errno -> String
string = enumString

byteArray :: Errno -> ByteArray
byteArray = enumByteArray

enumString :: Errno -> String
enumString (Errno i) =
  if fromIntegral i < (256 :: Word)
    then asString (PM.indexUnliftedArray enumTable (fromIntegral i))
    else unknownString

enumByteArray :: Errno -> ByteArray
enumByteArray (Errno i) =
  if fromIntegral i < (256 :: Word)
    then PM.indexUnliftedArray enumTable (fromIntegral i)
    else case unknown of
      ByteArray b -> ByteArray b

enumShortText :: Errno -> ShortText
enumShortText e = case enumByteArray e of
  ByteArray b -> TS.fromShortByteStringUnsafe (SBS b)

enumText :: Errno -> Text
enumText = TS.toText . enumShortText

descriptionString :: Errno -> String
descriptionString (Errno i) =
  if fromIntegral i < (256 :: Word)
    then asString (PM.indexUnliftedArray descriptionTable (fromIntegral i))
    else unknownString

descriptionByteArray :: Errno -> ByteArray
descriptionByteArray (Errno i) =
  if fromIntegral i < (256 :: Word)
    then PM.indexUnliftedArray descriptionTable (fromIntegral i)
    else case unknown of
      ByteArray b -> ByteArray b

descriptionShortText :: Errno -> ShortText
descriptionShortText e = case descriptionByteArray e of
  ByteArray b -> TS.fromShortByteStringUnsafe (SBS b)

descriptionText :: Errno -> Text
descriptionText = TS.toText . descriptionShortText

{-# NOINLINE unknown #-}
unknown :: ByteArray
unknown = asBytes unknownString

unknownString :: String
unknownString = "UNKNOWN"

enumTable :: UnliftedArray ByteArray
enumTable = runST $ do
  m <- PM.newUnliftedArray 256 (mempty :: ByteArray)
  for_ codes $ \(Description code descr _) -> do
    let ix = fromIntegral code :: Int
    Control.Monad.when ((ix < (256 :: Int)) && (ix >= 0)) $ PM.writeUnliftedArray m ix descr
  PM.unsafeFreezeUnliftedArray m

descriptionTable :: UnliftedArray ByteArray
descriptionTable = runST $ do
  m <- PM.newUnliftedArray 256 (mempty :: ByteArray)
  for_ codes $ \(Description code _ descr) -> do
    let ix = fromIntegral code :: Int
    when ((ix < (256 :: Int)) && (ix >= 0)) $ PM.writeUnliftedArray m ix descr
  PM.unsafeFreezeUnliftedArray m

data Description = Description !CInt !ByteArray !ByteArray

asBytes :: String -> ByteArray
asBytes s = runST $ do
  m <- PM.newByteArray (length s)
  for_ (zip (enumFrom (0 :: Int)) s) $ \(ix, c) -> do
    PM.writeByteArray m ix (charToWord8 c)
  PM.unsafeFreezeByteArray m

asString :: ByteArray -> String
asString = PM.foldrByteArray (\b cs -> word8ToChar b : cs) []

charToWord8 :: Char -> Word8
charToWord8 = fromIntegral . ord

word8ToChar :: Word8 -> Char
word8ToChar = chr . fromIntegral

codes :: SmallArray Description
codes =
  E.fromList
    [ {- 000 -} Description 0 (asBytes "EOK") (asBytes "OK")
    , {- 001 -} Description 1 (asBytes "EPERM") (asBytes "Operation not permitted")
    , {- 002 -} Description 2 (asBytes "ENOENT") (asBytes "No such file or directory")
    , {- 003 -} Description 3 (asBytes "ESRCH") (asBytes "No such process")
    , {- 004 -} Description 4 (asBytes "EINTR") (asBytes "Interrupted system call")
    , {- 005 -} Description 5 (asBytes "EIO") (asBytes "Input/output error")
    , {- 006 -} Description 6 (asBytes "ENXIO") (asBytes "No such device or address")
    , {- 007 -} Description 7 (asBytes "E2BIG") (asBytes "Argument list too long")
    , {- 008 -} Description 8 (asBytes "ENOEXEC") (asBytes "Exec format error")
    , {- 009 -} Description 9 (asBytes "EBADF") (asBytes "Bad file descriptor")
    , {- 010 -} Description 10 (asBytes "ECHILD") (asBytes "No child processes")
    , {- 011 -} Description 11 (asBytes "EAGAIN") (asBytes "Resource temporarily unavailable")
    , {- 012 -} Description 12 (asBytes "ENOMEM") (asBytes "Cannot allocate memory")
    , {- 013 -} Description 13 (asBytes "EACCES") (asBytes "Permission denied")
    , {- 014 -} Description 14 (asBytes "EFAULT") (asBytes "Bad address")
    , {- 015 -} Description 15 (asBytes "ENOTBLK") (asBytes "Block device required")
    , {- 016 -} Description 16 (asBytes "EBUSY") (asBytes "Device or resource busy")
    , {- 017 -} Description 17 (asBytes "EEXIST") (asBytes "File exists")
    , {- 018 -} Description 18 (asBytes "EXDEV") (asBytes "Invalid cross-device link")
    , {- 019 -} Description 19 (asBytes "ENODEV") (asBytes "No such device")
    , {- 020 -} Description 20 (asBytes "ENOTDIR") (asBytes "Not a directory")
    , {- 021 -} Description 21 (asBytes "EISDIR") (asBytes "Is a directory")
    , {- 022 -} Description 22 (asBytes "EINVAL") (asBytes "Invalid argument")
    , {- 023 -} Description 23 (asBytes "ENFILE") (asBytes "Too many open files in system")
    , {- 024 -} Description 24 (asBytes "EMFILE") (asBytes "Too many open files")
    , {- 025 -} Description 25 (asBytes "ENOTTY") (asBytes "Inappropriate ioctl for device")
    , {- 026 -} Description 26 (asBytes "ETXTBSY") (asBytes "Text file busy")
    , {- 027 -} Description 27 (asBytes "EFBIG") (asBytes "File too large")
    , {- 028 -} Description 28 (asBytes "ENOSPC") (asBytes "No space left on device")
    , {- 029 -} Description 29 (asBytes "ESPIPE") (asBytes "Illegal seek")
    , {- 030 -} Description 30 (asBytes "EROFS") (asBytes "Read-only file system")
    , {- 031 -} Description 31 (asBytes "EMLINK") (asBytes "Too many links")
    , {- 032 -} Description 32 (asBytes "EPIPE") (asBytes "Broken pipe")
    , {- 033 -} Description 33 (asBytes "EDOM") (asBytes "Numerical argument out of domain")
    , {- 034 -} Description 34 (asBytes "ERANGE") (asBytes "Numerical result out of range")
    , {- 035 -} Description 35 (asBytes "EDEADLK") (asBytes "Resource deadlock avoided")
    , {- 036 -} Description 36 (asBytes "ENAMETOOLONG") (asBytes "File name too long")
    , {- 037 -} Description 37 (asBytes "ENOLCK") (asBytes "No locks available")
    , {- 038 -} Description 38 (asBytes "ENOSYS") (asBytes "Function not implemented")
    , {- 039 -} Description 39 (asBytes "ENOTEMPTY") (asBytes "Directory not empty")
    , {- 040 -} Description 40 (asBytes "ELOOP") (asBytes "Too many levels of symbolic links")
    , {- 042 -} Description 42 (asBytes "ENOMSG") (asBytes "No message of desired type")
    , {- 043 -} Description 43 (asBytes "EIDRM") (asBytes "Identifier removed")
    , {- 044 -} Description 44 (asBytes "ECHRNG") (asBytes "Channel number out of range")
    , {- 045 -} Description 45 (asBytes "EL2NSYNC") (asBytes "Level 2 not synchronized")
    , {- 046 -} Description 46 (asBytes "EL3HLT") (asBytes "Level 3 halted")
    , {- 047 -} Description 47 (asBytes "EL3RST") (asBytes "Level 3 reset")
    , {- 048 -} Description 48 (asBytes "ELNRNG") (asBytes "Link number out of range")
    , {- 049 -} Description 49 (asBytes "EUNATCH") (asBytes "Protocol driver not attached")
    , {- 050 -} Description 50 (asBytes "ENOCSI") (asBytes "No CSI structure available")
    , {- 051 -} Description 51 (asBytes "EL2HLT") (asBytes "Level 2 halted")
    , {- 052 -} Description 52 (asBytes "EBADE") (asBytes "Invalid exchange")
    , {- 053 -} Description 53 (asBytes "EBADR") (asBytes "Invalid request descriptor")
    , {- 054 -} Description 54 (asBytes "EXFULL") (asBytes "Exchange full")
    , {- 055 -} Description 55 (asBytes "ENOANO") (asBytes "No anode")
    , {- 056 -} Description 56 (asBytes "EBADRQC") (asBytes "Invalid request code")
    , {- 057 -} Description 57 (asBytes "EBADSLT") (asBytes "Invalid slot")
    , {- 059 -} Description 59 (asBytes "EBFONT") (asBytes "Bad font file format")
    , {- 060 -} Description 60 (asBytes "ENOSTR") (asBytes "Device not a stream")
    , {- 061 -} Description 61 (asBytes "ENODATA") (asBytes "No data available")
    , {- 062 -} Description 62 (asBytes "ETIME") (asBytes "Timer expired")
    , {- 063 -} Description 63 (asBytes "ENOSR") (asBytes "Out of streams resources")
    , {- 064 -} Description 64 (asBytes "ENONET") (asBytes "Machine is not on the network")
    , {- 065 -} Description 65 (asBytes "ENOPKG") (asBytes "Package not installed")
    , {- 066 -} Description 66 (asBytes "EREMOTE") (asBytes "Object is remote")
    , {- 067 -} Description 67 (asBytes "ENOLINK") (asBytes "Link has been severed")
    , {- 068 -} Description 68 (asBytes "EADV") (asBytes "Advertise error")
    , {- 069 -} Description 69 (asBytes "ESRMNT") (asBytes "Srmount error")
    , {- 070 -} Description 70 (asBytes "ECOMM") (asBytes "Communication error on send")
    , {- 071 -} Description 71 (asBytes "EPROTO") (asBytes "Protocol error")
    , {- 072 -} Description 72 (asBytes "EMULTIHOP") (asBytes "Multihop attempted")
    , {- 073 -} Description 73 (asBytes "EDOTDOT") (asBytes "RFS specific error")
    , {- 074 -} Description 74 (asBytes "EBADMSG") (asBytes "Bad message")
    , {- 075 -} Description 75 (asBytes "EOVERFLOW") (asBytes "Value too large for defined data type")
    , {- 076 -} Description 76 (asBytes "ENOTUNIQ") (asBytes "Name not unique on network")
    , {- 077 -} Description 77 (asBytes "EBADFD") (asBytes "File descriptor in bad state")
    , {- 078 -} Description 78 (asBytes "EREMCHG") (asBytes "Remote address changed")
    , {- 079 -} Description 79 (asBytes "ELIBACC") (asBytes "Can not access a needed shared library")
    , {- 080 -} Description 80 (asBytes "ELIBBAD") (asBytes "Accessing a corrupted shared library")
    , {- 081 -} Description 81 (asBytes "ELIBSCN") (asBytes ".lib section in a.out corrupted")
    , {- 082 -} Description 82 (asBytes "ELIBMAX") (asBytes "Attempting to link in too many shared libraries")
    , {- 083 -} Description 83 (asBytes "ELIBEXEC") (asBytes "Cannot exec a shared library directly")
    , {- 084 -} Description 84 (asBytes "EILSEQ") (asBytes "Invalid or incomplete multibyte or wide character")
    , {- 085 -} Description 85 (asBytes "ERESTART") (asBytes "Interrupted system call should be restarted")
    , {- 086 -} Description 86 (asBytes "ESTRPIPE") (asBytes "Streams pipe error")
    , {- 087 -} Description 87 (asBytes "EUSERS") (asBytes "Too many users")
    , {- 088 -} Description 88 (asBytes "ENOTSOCK") (asBytes "Socket operation on non-socket")
    , {- 089 -} Description 89 (asBytes "EDESTADDRREQ") (asBytes "Destination address required")
    , {- 090 -} Description 90 (asBytes "EMSGSIZE") (asBytes "Message too long")
    , {- 091 -} Description 91 (asBytes "EPROTOTYPE") (asBytes "Protocol wrong type for socket")
    , {- 092 -} Description 92 (asBytes "ENOPROTOOPT") (asBytes "Protocol not available")
    , {- 093 -} Description 93 (asBytes "EPROTONOSUPPORT") (asBytes "Protocol not supported")
    , {- 094 -} Description 94 (asBytes "ESOCKTNOSUPPORT") (asBytes "Socket type not supported")
    , {- 095 -} Description 95 (asBytes "EOPNOTSUPP") (asBytes "Operation not supported")
    , {- 096 -} Description 96 (asBytes "EPFNOSUPPORT") (asBytes "Protocol family not supported")
    , {- 097 -} Description 97 (asBytes "EAFNOSUPPORT") (asBytes "Address family not supported by protocol")
    , {- 098 -} Description 98 (asBytes "EADDRINUSE") (asBytes "Address already in use")
    , {- 099 -} Description 99 (asBytes "EADDRNOTAVAIL") (asBytes "Cannot assign requested address")
    , {- 100 -} Description 100 (asBytes "ENETDOWN") (asBytes "Network is down")
    , {- 101 -} Description 101 (asBytes "ENETUNREACH") (asBytes "Network is unreachable")
    , {- 102 -} Description 102 (asBytes "ENETRESET") (asBytes "Network dropped connection on reset")
    , {- 103 -} Description 103 (asBytes "ECONNABORTED") (asBytes "Software caused connection abort")
    , {- 104 -} Description 104 (asBytes "ECONNRESET") (asBytes "Connection reset by peer")
    , {- 105 -} Description 105 (asBytes "ENOBUFS") (asBytes "No buffer space available")
    , {- 106 -} Description 106 (asBytes "EISCONN") (asBytes "Transport endpoint is already connected")
    , {- 107 -} Description 107 (asBytes "ENOTCONN") (asBytes "Transport endpoint is not connected")
    , {- 108 -} Description 108 (asBytes "ESHUTDOWN") (asBytes "Cannot send after transport endpoint shutdown")
    , {- 109 -} Description 109 (asBytes "ETOOMANYREFS") (asBytes "Too many references: cannot splice")
    , {- 110 -} Description 110 (asBytes "ETIMEDOUT") (asBytes "Connection timed out")
    , {- 111 -} Description 111 (asBytes "ECONNREFUSED") (asBytes "Connection refused")
    , {- 112 -} Description 112 (asBytes "EHOSTDOWN") (asBytes "Host is down")
    , {- 113 -} Description 113 (asBytes "EHOSTUNREACH") (asBytes "No route to host")
    , {- 114 -} Description 114 (asBytes "EALREADY") (asBytes "Operation already in progress")
    , {- 115 -} Description 115 (asBytes "EINPROGRESS") (asBytes "Operation now in progress")
    , {- 116 -} Description 116 (asBytes "ESTALE") (asBytes "Stale file handle")
    , {- 117 -} Description 117 (asBytes "EUCLEAN") (asBytes "Structure needs cleaning")
    , {- 118 -} Description 118 (asBytes "ENOTNAM") (asBytes "Not a XENIX named type file")
    , {- 119 -} Description 119 (asBytes "ENAVAIL") (asBytes "No XENIX semaphores available")
    , {- 120 -} Description 120 (asBytes "EISNAM") (asBytes "Is a named type file")
    , {- 121 -} Description 121 (asBytes "EREMOTEIO") (asBytes "Remote I/O error")
    , {- 122 -} Description 122 (asBytes "EDQUOT") (asBytes "Disk quota exceeded")
    , {- 123 -} Description 123 (asBytes "ENOMEDIUM") (asBytes "No medium found")
    , {- 124 -} Description 124 (asBytes "EMEDIUMTYPE") (asBytes "Wrong medium type")
    , {- 125 -} Description 125 (asBytes "ECANCELED") (asBytes "Operation canceled")
    , {- 126 -} Description 126 (asBytes "ENOKEY") (asBytes "Required key not available")
    , {- 127 -} Description 127 (asBytes "EKEYEXPIRED") (asBytes "Key has expired")
    , {- 128 -} Description 128 (asBytes "EKEYREVOKED") (asBytes "Key has been revoked")
    , {- 129 -} Description 129 (asBytes "EKEYREJECTED") (asBytes "Key was rejected by service")
    , {- 130 -} Description 130 (asBytes "EOWNERDEAD") (asBytes "Owner died")
    , {- 131 -} Description 131 (asBytes "ENOTRECOVERABLE") (asBytes "State not recoverable")
    , {- 132 -} Description 132 (asBytes "ERFKILL") (asBytes "Operation not possible due to RF-kill")
    , {- 133 -} Description 133 (asBytes "EHWPOISON") (asBytes "Memory page has hardware error")
    , {- 254 -} Description 254 (asBytes "EEOI") (asBytes "End of input")
    ]
