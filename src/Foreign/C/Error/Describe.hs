{-# language BangPatterns #-}

module Foreign.C.Error.Describe
  ( string
  , byteArray
  ) where

import Data.Char (ord,chr)
import Data.Primitive (SmallArray,UnliftedArray,ByteArray(..))
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import Foreign.C.Error (Errno(..))
import Data.Foldable (for_)
import Control.Monad.ST (runST)
import qualified Data.Primitive as PM
import qualified GHC.Exts as E
import qualified Foreign.C.Error as ERR

unErrno :: Errno -> CInt
unErrno (Errno i) = i


string :: Errno -> String
string (Errno i) = if fromIntegral i < (256 :: Word)
  then asString (PM.indexUnliftedArray table (fromIntegral i))
  else unknownString

byteArray :: Errno -> ByteArray
byteArray (Errno i) = if fromIntegral i < (256 :: Word)
  then PM.indexUnliftedArray table (fromIntegral i)
  else case unknown of
    ByteArray b -> ByteArray b

{-# NOINLINE unknown #-}
unknown :: ByteArray
unknown = asBytes unknownString

unknownString :: String
unknownString = "UNKNOWN"

table :: UnliftedArray ByteArray
table = runST $ do
  m <- PM.newUnliftedArray 256 (mempty :: ByteArray)
  for_ codes $ \(Description code descr) -> do
    let ix = fromIntegral code :: Int
    if ix < (256 :: Int)
      then if ix >= 0
        then PM.writeUnliftedArray m ix descr
        else pure ()
      else pure ()
  PM.unsafeFreezeUnliftedArray m

data Description = Description !CInt !ByteArray

asBytes :: String -> ByteArray
asBytes s = runST $ do
  m <- PM.newByteArray (length s)
  for_ (zip (enumFrom (0 :: Int)) s) $ \(ix,c) -> do
    PM.writeByteArray m ix (charToWord8 c)
  PM.unsafeFreezeByteArray m

asString :: ByteArray -> String
asString = PM.foldrByteArray (\b cs -> word8ToChar b : cs) []

charToWord8 :: Char -> Word8
charToWord8 = fromIntegral . ord

word8ToChar :: Word8 -> Char
word8ToChar = chr . fromIntegral

codes :: SmallArray Description
codes = E.fromList
  [ Description (unErrno ERR.eOK) (asBytes "EOK")
  , Description (unErrno ERR.e2BIG) (asBytes "E2BIG")
  , Description (unErrno ERR.eACCES) (asBytes "EACCES")
  , Description (unErrno ERR.eADDRINUSE) (asBytes "EADDRINUSE")
  , Description (unErrno ERR.eADDRNOTAVAIL) (asBytes "EADDRNOTAVAIL")
  , Description (unErrno ERR.eADV) (asBytes "EADV")
  , Description (unErrno ERR.eAFNOSUPPORT) (asBytes "EAFNOSUPPORT")
  , Description (unErrno ERR.eAGAIN) (asBytes "EAGAIN")
  , Description (unErrno ERR.eALREADY) (asBytes "EALREADY")
  , Description (unErrno ERR.eBADF) (asBytes "EBADF")
  , Description (unErrno ERR.eBADMSG) (asBytes "EBADMSG")
  , Description (unErrno ERR.eBADRPC) (asBytes "EBADRPC")
  , Description (unErrno ERR.eBUSY) (asBytes "EBUSY")
  , Description (unErrno ERR.eCHILD) (asBytes "ECHILD")
  , Description (unErrno ERR.eCOMM) (asBytes "ECOMM")
  , Description (unErrno ERR.eCONNABORTED) (asBytes "ECONNABORTED")
  , Description (unErrno ERR.eCONNREFUSED) (asBytes "ECONNREFUSED")
  , Description (unErrno ERR.eCONNRESET) (asBytes "ECONNRESET")
  , Description (unErrno ERR.eDEADLK) (asBytes "EDEADLK")
  , Description (unErrno ERR.eDESTADDRREQ) (asBytes "EDESTADDRREQ")
  , Description (unErrno ERR.eDIRTY) (asBytes "EDIRTY")
  , Description (unErrno ERR.eDOM) (asBytes "EDOM")
  , Description (unErrno ERR.eDQUOT) (asBytes "EDQUOT")
  , Description (unErrno ERR.eEXIST) (asBytes "EEXIST")
  , Description (unErrno ERR.eFAULT) (asBytes "EFAULT")
  , Description (unErrno ERR.eFBIG) (asBytes "EFBIG")
  , Description (unErrno ERR.eFTYPE) (asBytes "EFTYPE")
  , Description (unErrno ERR.eHOSTDOWN) (asBytes "EHOSTDOWN")
  , Description (unErrno ERR.eHOSTUNREACH) (asBytes "EHOSTUNREACH")
  , Description (unErrno ERR.eIDRM) (asBytes "EIDRM")
  , Description (unErrno ERR.eILSEQ) (asBytes "EILSEQ")
  , Description (unErrno ERR.eINPROGRESS) (asBytes "EINPROGRESS")
  , Description (unErrno ERR.eINTR) (asBytes "EINTR")
  , Description (unErrno ERR.eINVAL) (asBytes "EINVAL")
  , Description (unErrno ERR.eIO) (asBytes "EIO")
  , Description (unErrno ERR.eISCONN) (asBytes "EISCONN")
  , Description (unErrno ERR.eISDIR) (asBytes "EISDIR")
  , Description (unErrno ERR.eLOOP) (asBytes "ELOOP")
  , Description (unErrno ERR.eMFILE) (asBytes "EMFILE")
  , Description (unErrno ERR.eMLINK) (asBytes "EMLINK")
  , Description (unErrno ERR.eMSGSIZE) (asBytes "EMSGSIZE")
  , Description (unErrno ERR.eMULTIHOP) (asBytes "EMULTIHOP")
  , Description (unErrno ERR.eNAMETOOLONG) (asBytes "ENAMETOOLONG")
  , Description (unErrno ERR.eNETDOWN) (asBytes "ENETDOWN")
  , Description (unErrno ERR.eNETRESET) (asBytes "ENETRESET")
  , Description (unErrno ERR.eNETUNREACH) (asBytes "ENETUNREACH")
  , Description (unErrno ERR.eNFILE) (asBytes "ENFILE")
  , Description (unErrno ERR.eNOBUFS) (asBytes "ENOBUFS")
  , Description (unErrno ERR.eNODATA) (asBytes "ENODATA")
  , Description (unErrno ERR.eNODEV) (asBytes "ENODEV")
  , Description (unErrno ERR.eNOENT) (asBytes "ENOENT")
  , Description (unErrno ERR.eNOEXEC) (asBytes "ENOEXEC")
  , Description (unErrno ERR.eNOLCK) (asBytes "ENOLCK")
  , Description (unErrno ERR.eNOLINK) (asBytes "ENOLINK")
  , Description (unErrno ERR.eNOMEM) (asBytes "ENOMEM")
  , Description (unErrno ERR.eNOMSG) (asBytes "ENOMSG")
  , Description (unErrno ERR.eNONET) (asBytes "ENONET")
  , Description (unErrno ERR.eNOPROTOOPT) (asBytes "ENOPROTOOPT")
  , Description (unErrno ERR.eNOSPC) (asBytes "ENOSPC")
  , Description (unErrno ERR.eNOSR) (asBytes "ENOSR")
  , Description (unErrno ERR.eNOSTR) (asBytes "ENOSTR")
  , Description (unErrno ERR.eNOSYS) (asBytes "ENOSYS")
  , Description (unErrno ERR.eNOTBLK) (asBytes "ENOTBLK")
  , Description (unErrno ERR.eNOTCONN) (asBytes "ENOTCONN")
  , Description (unErrno ERR.eNOTDIR) (asBytes "ENOTDIR")
  , Description (unErrno ERR.eNOTEMPTY) (asBytes "ENOTEMPTY")
  , Description (unErrno ERR.eNOTSOCK) (asBytes "ENOTSOCK")
  , Description (unErrno ERR.eNOTSUP) (asBytes "ENOTSUP")
  , Description (unErrno ERR.eNOTTY) (asBytes "ENOTTY")
  , Description (unErrno ERR.eNXIO) (asBytes "ENXIO")
  , Description (unErrno ERR.eOPNOTSUPP) (asBytes "EOPNOTSUPP")
  , Description (unErrno ERR.ePERM) (asBytes "EPERM")
  , Description (unErrno ERR.ePFNOSUPPORT) (asBytes "EPFNOSUPPORT")
  , Description (unErrno ERR.ePIPE) (asBytes "EPIPE")
  , Description (unErrno ERR.ePROCLIM) (asBytes "EPROCLIM")
  , Description (unErrno ERR.ePROCUNAVAIL) (asBytes "EPROCUNAVAIL")
  , Description (unErrno ERR.ePROGMISMATCH) (asBytes "EPROGMISMATCH")
  , Description (unErrno ERR.ePROGUNAVAIL) (asBytes "EPROGUNAVAIL")
  , Description (unErrno ERR.ePROTO) (asBytes "EPROTO")
  , Description (unErrno ERR.ePROTONOSUPPORT) (asBytes "EPROTONOSUPPORT")
  , Description (unErrno ERR.ePROTOTYPE) (asBytes "EPROTOTYPE")
  , Description (unErrno ERR.eRANGE) (asBytes "ERANGE")
  , Description (unErrno ERR.eREMCHG) (asBytes "EREMCHG")
  , Description (unErrno ERR.eREMOTE) (asBytes "EREMOTE")
  , Description (unErrno ERR.eROFS) (asBytes "EROFS")
  , Description (unErrno ERR.eRPCMISMATCH) (asBytes "ERPCMISMATCH")
  , Description (unErrno ERR.eRREMOTE) (asBytes "ERREMOTE")
  , Description (unErrno ERR.eSHUTDOWN) (asBytes "ESHUTDOWN")
  , Description (unErrno ERR.eSOCKTNOSUPPORT) (asBytes "ESOCKTNOSUPPORT")
  , Description (unErrno ERR.eSPIPE) (asBytes "ESPIPE")
  , Description (unErrno ERR.eSRCH) (asBytes "ESRCH")
  , Description (unErrno ERR.eSRMNT) (asBytes "ESRMNT")
  , Description (unErrno ERR.eSTALE) (asBytes "ESTALE")
  , Description (unErrno ERR.eTIME) (asBytes "ETIME")
  , Description (unErrno ERR.eTIMEDOUT) (asBytes "ETIMEDOUT")
  , Description (unErrno ERR.eTOOMANYREFS) (asBytes "ETOOMANYREFS")
  , Description (unErrno ERR.eTXTBSY) (asBytes "ETXTBSY")
  , Description (unErrno ERR.eUSERS) (asBytes "EUSERS")
  , Description (unErrno ERR.eWOULDBLOCK) (asBytes "EWOULDBLOCK")
  , Description (unErrno ERR.eXDEV) (asBytes "EXDEV")
  ]
