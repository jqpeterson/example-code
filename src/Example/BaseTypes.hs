{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Example.BaseTypes where

import Control.Exception       (Exception,SomeException)
import Control.Monad           (replicateM)
import Data.Bimap              (Bimap,fromList,lookupR)
import qualified Data.Bimap as BM
import Data.ByteString         (ByteString,concat)
import Data.ByteString.Char8   (head,last,singleton)
import Data.Foldable           (Foldable,foldl')
import Data.Int                (Int,Int16,Int32,Int64)
import Data.List               (zip)
import Data.Map.Strict         (Map,lookup)
import Data.Serialize          (Get,Put,Result(..)
                               ,getByteString,getInt16be,getInt16le,getInt32be
                               ,getInt32le,getInt64be,getInt64le
                               ,getFloat32be,getFloat32le,getFloat64be,getFloat64le
                               ,getWord8,getWord16be,getWord16le
                               ,getWord32be,getWord32le,getWord64be,getWord64le
                               ,runGet,runPut
                               ,putByteString,putInt16be,putInt16le,putInt32be
                               ,putInt32le,putInt64be,putInt64le
                               ,putFloat32be,putFloat32le,putFloat64be,putFloat64le
                               ,putWord8,putWord16be,putWord16le
                               ,putWord32be,putWord32le,putWord64be,putWord64le
                               ,runGetPartial)
import Data.Sequence           (Seq,(|>),empty)
import Data.Typeable           (Typeable)
import Data.Word               (Word8,Word16,Word32,Word64)
import Numeric.Interval        (Interval)
import Prelude                 (Char,Double,Eq,Either(..),Enum,Float,Maybe(..),Ord,Show,String
                               ,($),(.),(<$>),(=<<),(++),(>>),(+),(<),(==)
                               ,either,fail,otherwise,pure,return,reverse,show,traverse)


--------- Explicitly define that Strict ByteStrings are being used ----------

type StrictByteString = ByteString
-------------------------------------------------------

-------- Configuration of data where the map is passed in to allow for varying configurations --------- 
data ExampleConfiguration idx = ExampleConfiguration { _masking      :: Masking
                                                     , _endianness   :: Endianness
                                                     , _indexMapping :: Map idx (ExampleMapping idx)
                                                     } deriving (Eq,Ord,Show)
                         
data Endianness = BigEndian
                | LittleEndian
                  deriving (Eq,Ord,Show)

data Masking = Masked
             | NotMasked
               deriving (Eq,Ord,Show)
                        
-- | ExampleMapping is the core of the parsing paradigm. The parameterized index allows external indexing of each parameter being parsed,
-- | while ExampleParameterType defines the data size, and the Interval ExampleAddress defines how the data is Addressed locally.
-- | This is important as each application instance assigns memory differently and sometimes without conformity.
data ExampleMapping idx = ExampleMapping { _deviceIndex            :: idx
                                         , _parameterType          :: ExampleParameterType
                                         , _exampleAddressInterval :: Interval ExampleAddress
                                         } deriving (Eq,Ord,Show)


--------------------------------- Type definition for types of Address -----------------
data ExampleAddress = AddressA Word16
                    | AddressB Word16
                    | AddressC Word16
                    | AddressD Word16
                    | AddressE Word16
                    | AddressF Word16
                     deriving (Eq,Ord,Show)

--------------------------------- Defines data size without storing data -------------------------                              
data ExampleParameterType = Word8Parameter
                          | Word16Parameter
                          | Word32Parameter
                          | Word64Parameter
                          | Int16Parameter
                          | Int32Parameter
                          | Int64Parameter
                          | FloatParameter
                          | DoubleParameter
                           deriving (Eq,Ord,Show)                                           

---------------------------------- Converter to Word8 to allow for computing bytestring size/parsing limits -----------------                                    
parameterCost :: ExampleParameterType -> Word8
parameterCost Word8Parameter  = 1
parameterCost Word16Parameter = 2
parameterCost Word32Parameter = 4
parameterCost Word64Parameter = 8
parameterCost Int16Parameter  = 2
parameterCost Int32Parameter  = 4
parameterCost Int64Parameter  = 8
parameterCost FloatParameter  = 4
parameterCost DoubleParameter = 8

------ Converter from DataType with stored values to a data size representation. Good for equality checks or serializing requests ...........
convertDataToParameterType :: ExampleData -> ExampleParameterType
convertDataToParameterType (Word8Data _)   = Word8Parameter
convertDataToParameterType (Word16Data _)  = Word16Parameter
convertDataToParameterType (Word32Data _)  = Word32Parameter
convertDataToParameterType (Word64Data _)  = Word64Parameter
convertDataToParameterType (Int16Data _)   = Int16Parameter
convertDataToParameterType (Int32Data _)   = Int32Parameter
convertDataToParameterType (Int64Data _)   = Int64Parameter
convertDataToParameterType (FloatData _)   = FloatParameter
convertDataToParameterType (DoubleData _)  = DoubleParameter
                                            
----------------------------- ExampleData Types and Functions -------------------------

--------------------- Data Types with stored values wrapped in to a single type -----------------------------
data ExampleData = Word8Data  Word8
                 | Word16Data Word16
                 | Word32Data Word32
                 | Word64Data Word64
                 | Int16Data  Int16
                 | Int32Data  Int32
                 | Int64Data  Int64
                 | FloatData  Float
                 | DoubleData Double
                  deriving (Eq,Ord,Show)

------------ Converter to data size to compute bytestring sizes when sending data ---------------------------
exampleDataCost :: ExampleData -> Word8
exampleDataCost (Word8Data _)   = 1
exampleDataCost (Word16Data _)  = 2
exampleDataCost (Word32Data _)  = 4
exampleDataCost (Word64Data _)  = 8
exampleDataCost (Int16Data _)   = 2
exampleDataCost (Int32Data _)   = 4
exampleDataCost (Int64Data _)   = 8
exampleDataCost (FloatData _)   = 4
exampleDataCost (DoubleData _)  = 8

-- Function to take any data structure with a foldable instance containing Example Data and computing the size of the data contained inside ---
computeTotalExampleDataCost :: (Foldable t) => t ExampleData -> Either String Word8
computeTotalExampleDataCost = foldl' computeCost (Right 0)
  where
    computeCost :: Either String Word8 -> ExampleData -> Either String Word8
    computeCost (Left err) _ = Left err
    computeCost (Right cost) exampledata
        | cost + exampleDataCost exampledata < cost = Left "Number of bytes required exceeded the 256 bytes limit."
        | otherwise = Right $ cost + exampleDataCost exampledata

---------- Function that uses the exampleconfiguration to create a get (deserializer) to a ExampleData type -----------------
getExampleData :: (Ord a,Show a) => ExampleConfiguration a -> ExampleParameterType -> Get ExampleData
getExampleData exampleconfig Word8Parameter  = Word8Data  <$> exampledataget exampleconfig
getExampleData exampleconfig Word16Parameter = Word16Data <$> exampledataget exampleconfig
getExampleData exampleconfig Word32Parameter = Word32Data <$> exampledataget exampleconfig
getExampleData exampleconfig Word64Parameter = Word64Data <$> exampledataget exampleconfig
getExampleData exampleconfig Int16Parameter  = Int16Data  <$> exampledataget exampleconfig
getExampleData exampleconfig Int32Parameter  = Int32Data  <$> exampledataget exampleconfig
getExampleData exampleconfig Int64Parameter  = Int64Data  <$> exampledataget exampleconfig
getExampleData exampleconfig FloatParameter  = FloatData  <$> exampledataget exampleconfig
getExampleData exampleconfig DoubleParameter = DoubleData <$> exampledataget exampleconfig

---------- Function that uses the exampleconfiguration to create a bytestring (serializer) from a ExampleData type -----------------
putExampleData :: (Ord a,Show a) => ExampleConfiguration a -> ExampleData -> Put
putExampleData exampleconfig (Word8Data  w8 ) = exampledataput exampleconfig w8
putExampleData exampleconfig (Word16Data w16) = exampledataput exampleconfig w16
putExampleData exampleconfig (Word32Data w32) = exampledataput exampleconfig w32
putExampleData exampleconfig (Word64Data w64) = exampledataput exampleconfig w64
putExampleData exampleconfig (Int16Data  i16) = exampledataput exampleconfig i16
putExampleData exampleconfig (Int32Data  i32) = exampledataput exampleconfig i32
putExampleData exampleconfig (Int64Data  i64) = exampledataput exampleconfig i64
putExampleData exampleconfig (FloatData  f  ) = exampledataput exampleconfig f
putExampleData exampleconfig (DoubleData d  ) = exampledataput exampleconfig d   

-------------- Function that take 
getSeqOfExampleData :: (Ord a,Show a) => ExampleConfiguration a -> Seq a -> Get (Seq ExampleData)
getSeqOfExampleData exampleconfig@(ExampleConfiguration _ _ indicesmap) seqindices =
  case foldl' (getExampleParameterTypes indicesmap) (Right empty) seqindices of
    Left err -> fail err
    Right seqparamtypes -> getExampleData exampleconfig `traverse` seqparamtypes

getExampleParameterTypes :: (Ord a,Show a) => Map a (ExampleMapping a) -> Either String (Seq ExampleParameterType) -> a -> Either String (Seq ExampleParameterType)
getExampleParameterTypes _ err@(Left _) _ = err
getExampleParameterTypes indicesmap (Right seqexampleparamtype) idx =
  case lookup idx indicesmap of
    Nothing -> Left $ "Unable to find Parameter Index: " ++ show idx
    Just (ExampleMapping _ parametertype _) -> Right $ seqexampleparamtype |> parametertype
                                
-----------------------------------------------------------------------------
class (ExamplePut a, ExampleGet a) => ExampleType a where

class ExampleGet a where
    exampleget :: (Ord b, Show b) => ExampleConfiguration b -> Seq b -> Get a
    
class ExamplePut a where
    exampleput :: (Ord b, Show b) => ExampleConfiguration b -> a -> Put

-------------------------------------------------------------------------------------
class ExampleResponseType  (m :: * -> *)  where
    exampleresponseget :: (Ord a, Show a) => ExampleConfiguration a -> Seq a -> Get (m a)
    exampleresponseput :: (Ord a, Show a) => ExampleConfiguration a -> m a -> Put

--------------------------------------------------------------------------------------

------------------ Type Class defined for serialization of data values ----------------                           
class (ExampleDataPut a, ExampleDataGet a) => ExampleDataType a where

class ExampleDataGet a where
    exampledataget :: (Ord b,Show b) => ExampleConfiguration b -> Get a
    
class ExampleDataPut a where
    exampledataput :: (Ord b,Show b) => ExampleConfiguration b -> a -> Put
---------------------------------------------------------------------------------------
                 
------------------ ExampleDataType Instances of basic data types ----------------------
instance ExampleDataType Word8

instance ExampleDataGet Word8 where
    exampledataget _  = getWord8

instance ExampleDataPut Word8 where
    exampledataput _ = putWord8
                  
instance ExampleDataType Char

instance ExampleDataGet Char where
    exampledataget (ExampleConfiguration _ BigEndian _)    = head <$> getByteString 2
    exampledataget (ExampleConfiguration _ LittleEndian _) = last <$> getByteString 2
                  
instance ExampleDataPut Char where
    exampledataput (ExampleConfiguration _ BigEndian _) c    = putByteString (singleton c) >> putWord8 0x00
    exampledataput (ExampleConfiguration _ LittleEndian _) c = putWord8 0x00 >> putByteString (singleton c)

                                                         
instance ExampleDataType Word16

instance ExampleDataGet Word16 where
    exampledataget (ExampleConfiguration _ BigEndian _)    = getWord16be
    exampledataget (ExampleConfiguration _ LittleEndian _) = getWord16le
                                                       
instance ExampleDataPut Word16 where
    exampledataput (ExampleConfiguration _ BigEndian _)    = putWord16be
    exampledataput (ExampleConfiguration _ LittleEndian _) = putWord16le

                                 
instance ExampleDataType Word32

instance ExampleDataGet Word32 where
    exampledataget (ExampleConfiguration NotMasked BigEndian _)    = getWord32be
    exampledataget (ExampleConfiguration NotMasked LittleEndian _) = getWord32le
    exampledataget (ExampleConfiguration Masked BigEndian _)       = getMaskedBytes 2 getWord32be
    exampledataget (ExampleConfiguration Masked LittleEndian _)    = getMaskedBytes 2 getWord32le
                                                                
instance ExampleDataPut Word32 where
    exampledataput (ExampleConfiguration  NotMasked BigEndian _)    = putWord32be
    exampledataput (ExampleConfiguration  NotMasked LittleEndian _) = putWord32le
    exampledataput (ExampleConfiguration  Masked BigEndian _)       = putMaskedBytes 2 putWord32be
    exampledataput (ExampleConfiguration  Masked LittleEndian _)    = putMaskedBytes 2 putWord32le

                                 
instance ExampleDataType Word64

instance ExampleDataGet Word64 where
    exampledataget (ExampleConfiguration NotMasked BigEndian _)    = getWord64be
    exampledataget (ExampleConfiguration NotMasked LittleEndian _) = getWord64le
    exampledataget (ExampleConfiguration Masked BigEndian _)       = getMaskedBytes 4 getWord64be
    exampledataget (ExampleConfiguration Masked LittleEndian _)    = getMaskedBytes 4 getWord64le

instance ExampleDataPut Word64 where
    exampledataput (ExampleConfiguration NotMasked BigEndian _)    = putWord64be
    exampledataput (ExampleConfiguration NotMasked LittleEndian _) = putWord64le
    exampledataput (ExampleConfiguration Masked BigEndian _)       = putMaskedBytes 4 putWord64be
    exampledataput (ExampleConfiguration Masked LittleEndian _)    = putMaskedBytes 4 putWord64le
                                 
instance ExampleDataType Int16

instance ExampleDataGet Int16 where
    exampledataget (ExampleConfiguration _ BigEndian _)    = getInt16be
    exampledataget (ExampleConfiguration _ LittleEndian _) = getInt16le

instance ExampleDataPut Int16 where
    exampledataput (ExampleConfiguration _ BigEndian _)    = putInt16be
    exampledataput (ExampleConfiguration _ LittleEndian _) = putInt16le

                                 
instance ExampleDataType Int32  

instance ExampleDataGet Int32 where  
    exampledataget (ExampleConfiguration NotMasked BigEndian _)    = getInt32be
    exampledataget (ExampleConfiguration NotMasked LittleEndian _) = getInt32le
    exampledataget (ExampleConfiguration Masked BigEndian _)       = getMaskedBytes 2 getInt32be
    exampledataget (ExampleConfiguration Masked LittleEndian _)    = getMaskedBytes 2 getInt32le

instance ExampleDataPut Int32 where
    exampledataput (ExampleConfiguration NotMasked BigEndian _)    = putInt32be
    exampledataput (ExampleConfiguration NotMasked LittleEndian _) = putInt32le
    exampledataput (ExampleConfiguration Masked BigEndian _)       = putMaskedBytes 2 putInt32be
    exampledataput (ExampleConfiguration Masked LittleEndian _)    = putMaskedBytes 2 putInt32le
               
                                                                
instance ExampleDataType Int64

instance ExampleDataGet Int64 where
    exampledataget (ExampleConfiguration NotMasked BigEndian _)    = getInt64be
    exampledataget (ExampleConfiguration NotMasked LittleEndian _) = getInt64le
    exampledataget (ExampleConfiguration Masked BigEndian _)       = getMaskedBytes 4 getInt64be
    exampledataget (ExampleConfiguration Masked LittleEndian _)    = getMaskedBytes 4 getInt64le
                                                       
instance ExampleDataPut Int64 where
    exampledataput (ExampleConfiguration NotMasked BigEndian _)    = putInt64be
    exampledataput (ExampleConfiguration NotMasked LittleEndian _) = putInt64le
    exampledataput (ExampleConfiguration Masked BigEndian _)       = putMaskedBytes 4 putInt64be
    exampledataput (ExampleConfiguration Masked LittleEndian _)    = putMaskedBytes 4 putInt64le

                
instance ExampleDataType Float

instance ExampleDataGet Float where
    exampledataget (ExampleConfiguration NotMasked BigEndian _)    = getFloat32be
    exampledataget (ExampleConfiguration NotMasked LittleEndian _) = getFloat32le
    exampledataget (ExampleConfiguration Masked BigEndian _)       = getMaskedBytes 2 getFloat32be
    exampledataget (ExampleConfiguration Masked LittleEndian _)    = getMaskedBytes 2 getFloat32le

                                                                
instance ExampleDataPut Float where
    exampledataput (ExampleConfiguration NotMasked BigEndian _)    = putFloat32be
    exampledataput (ExampleConfiguration NotMasked LittleEndian _) = putFloat32le
    exampledataput (ExampleConfiguration Masked BigEndian _)       = putMaskedBytes 2 putFloat32be
    exampledataput (ExampleConfiguration Masked LittleEndian _)    = putMaskedBytes 2 putFloat32le

                                 
instance ExampleDataType Double

instance ExampleDataGet Double where
    exampledataget (ExampleConfiguration NotMasked BigEndian _)    = getFloat64be
    exampledataget (ExampleConfiguration NotMasked LittleEndian _) = getFloat64le
    exampledataget (ExampleConfiguration Masked BigEndian _)       = getMaskedBytes 4 getFloat64be
    exampledataget (ExampleConfiguration Masked LittleEndian _)    = getMaskedBytes 4 getFloat64le

instance ExampleDataPut Double where
    exampledataput (ExampleConfiguration NotMasked BigEndian _)    = putFloat64be
    exampledataput (ExampleConfiguration NotMasked LittleEndian _) = putFloat64le    
    exampledataput (ExampleConfiguration Masked BigEndian _)       = putMaskedBytes 4 putFloat64be
    exampledataput (ExampleConfiguration Masked LittleEndian _)    = putMaskedBytes 4 putFloat64le
-----------------------------------------------------------------------------------------------------

------------------------- Bit Operations ------------------------------------------------------------

data BitState = BitOn
              | BitOff
                 deriving (Eq,Ord,Show)

instance ExampleDataType BitState

instance ExampleDataGet BitState where
    exampledataget (ExampleConfiguration _ BigEndian _)    = convertToBitState =<< getWord16be
    exampledataget (ExampleConfiguration _ LittleEndian _) = convertToBitState =<< getWord16le

convertToBitState :: Word16 -> Get BitState
convertToBitState 0x0000 = pure BitOff
convertToBitState 0xFF00 = pure BitOn
convertToBitState x      = fail $ "Expected either 0x0000 or 0xFF00 for BitState, but received " ++ show x

convertBitToExampleData :: BitState -> ExampleData
convertBitToExampleData BitOn  = Word16Data 0xFF00
convertBitToExampleData BitOff = Word16Data 0x0000
                            
instance ExampleDataPut BitState where
    exampledataput (ExampleConfiguration _ BigEndian _)    BitOn  = putWord16be 0xFF00
    exampledataput (ExampleConfiguration _ LittleEndian _) BitOn  = putWord16le 0xFF00
    exampledataput (ExampleConfiguration _ BigEndian _)    BitOff = putWord16be 0x0000
    exampledataput (ExampleConfiguration _ LittleEndian _) BitOff = putWord16le 0x0000                                                           
               
getMaskedBytes :: Int -> Get a -> Get a
getMaskedBytes chunks getendian = do
  bytechunks <- replicateM chunks (getByteString 2)
  either fail return (runGet getendian (concat . reverse $ bytechunks))

putMaskedBytes :: Int -> (a -> Put) -> a -> Put
putMaskedBytes chunks putendian d =
  either fail (putByteString . concat . reverse) (bsfinal bs)
  where
    bs = runPut (putendian d)
    bsfinal = runGet (replicateM chunks (getByteString 2))


--------------------------------- Server Exception Responses -------------------------------------------------------
              
data ExampleException = ExampleException { _exceptionCode          :: ExceptionCode
                                         , _exampleExceptionCode   :: ExampleExceptionCode
                                         } deriving (Eq,Ord,Show)

getExampleException :: (Ord a, Show a) => ExampleConfiguration a -> Word8 -> Get ExampleException
getExampleException exampleconfig exceptioncode =
  case BM.lookup exceptioncode exceptionCodeBimap of
    Nothing -> fail $ "Failed to parse ExampleException due to unrecognized Code: " ++ show exceptioncode
    Just exceptioncode' -> do
      exampleexceptioncode <- exampledataget exampleconfig
      return $ ExampleException exceptioncode' exampleexceptioncode


putExampleException :: (Ord a,Show a) => ExampleConfiguration a -> ExampleException -> Put
putExampleException exampleconfig (ExampleException exceptioncode exampleexceptioncode) =
    case lookupR exceptioncode exceptionCodeBimap of
      Nothing -> fail $ "Unable to serialize ExampleException due to unknown ExceptionCode: " ++ show exceptioncode
      Just word8 -> do
        exampledataput exampleconfig word8
        exampledataput exampleconfig exampleexceptioncode

------------------------- Allows lookup of Named ExceptionCode with parsed word8 -------------------------------------
exceptionCodeBimap :: Bimap Word8 ExceptionCode
exceptionCodeBimap = fromList $ zip word8list exceptionlist 
    where
      word8list     = [0x81,0x82,0x83,0x84,0x85,0x86,0x87,0x88,0x8B,0x8C,0x8F,0x90,0x91,0x94,0x95,0x96,0x97,0x98,0xAB]
      exceptionlist = [ExceptionCodeA ..]
      
                                   
data ExceptionCode = ExceptionCodeA
                   | ExceptionCodeB
                   | ExceptionCodeC
                   | ExceptionCodeD
                   | ExceptionCodeE
                   | ExceptionCodeF
                   | ExceptionCodeG
                   | ExceptionCodeH
                   | ExceptionCodeI
                   | ExceptionCodeJ
                   | ExceptionCodeK
                   | ExceptionCodeL
                   | ExceptionCodeM
                   | ExceptionCodeN
                   | ExceptionCodeO
                   | ExceptionCodeP
                   | ExceptionCodeQ
                   | ExceptionCodeR
                   | ExceptionCodeS
                     deriving (Enum,Eq,Ord,Show)

----------------------------- Defines the specific error for ExceptionCode -----------------------
data ExampleExceptionCode = SpecificReasonxA
                          | SpecificReasonxB
                          | SpecificReasonxC
                          | SpecificReasonxD
                          | SpecificReasonxE
                          | SpecificReasonxF
                          | SpecificReasonxG
                          | SpecificReasonxH
                          | SpecificReasonxI
                            deriving (Eq,Ord,Show)

----------------------------- ExampleDataType Instances for ExampleException ---------------------
instance ExampleDataType ExampleExceptionCode
                                
instance ExampleDataGet ExampleExceptionCode where
    exampledataget exampleconfig = do
      exceptioncode <- exampledataget exampleconfig :: Get Word8
      case exceptioncode of
        1  -> return SpecificReasonxA
        2  -> return SpecificReasonxB
        3  -> return SpecificReasonxC
        4  -> return SpecificReasonxD
        5  -> return SpecificReasonxE
        6  -> return SpecificReasonxF
        8  -> return SpecificReasonxG
        10 -> return SpecificReasonxH
        11 -> return SpecificReasonxI
        x  -> fail $ "Unknown Example Exception Code: " ++ show x

instance ExampleDataPut ExampleExceptionCode where
    exampledataput exampleconfig SpecificReasonxA = exampledataput exampleconfig (1  :: Word8)
    exampledataput exampleconfig SpecificReasonxB = exampledataput exampleconfig (2  :: Word8)
    exampledataput exampleconfig SpecificReasonxC = exampledataput exampleconfig (3  :: Word8)
    exampledataput exampleconfig SpecificReasonxD = exampledataput exampleconfig (4  :: Word8)
    exampledataput exampleconfig SpecificReasonxE = exampledataput exampleconfig (5  :: Word8)
    exampledataput exampleconfig SpecificReasonxF = exampledataput exampleconfig (6  :: Word8)
    exampledataput exampleconfig SpecificReasonxG = exampledataput exampleconfig (8  :: Word8)
    exampledataput exampleconfig SpecificReasonxH = exampledataput exampleconfig (10 :: Word8)
    exampledataput exampleconfig SpecificReasonxI = exampledataput exampleconfig (11 :: Word8)


                                                                    
-----------------------------------------------------------------------------------

newtype BitQuantity = BitQuantity {_unBitQuantity :: Word16} deriving (Eq,ExampleDataGet,ExampleDataPut,Ord,Show)

newtype DeviceIdCode = DeviceIdCode {_unObjectIdCode :: Word8} deriving (Eq,ExampleDataGet,ExampleDataPut,Ord,Show)

newtype DiagnosticRegister = DiagnosticRegister {_unDiagnosticRegister :: Word16} deriving (Eq,ExampleDataGet,ExampleDataPut,Ord,Show)
   
newtype ExceptionStatusWord = ExceptionStatusWord {_unExceptionStatusWord :: Word16} deriving (Eq,ExampleDataGet,ExampleDataPut,Ord,Show)
    
newtype ObjectIdNumber = ObjectIdNumber {_unObjectIdNumber :: Word8} deriving (Eq,ExampleDataGet,ExampleDataPut,Ord,Show)
       
newtype ReferenceType = ReferenceType {_unReferenceType :: Word8} deriving (Eq,ExampleDataGet,ExampleDataPut,Ord,Show)

newtype RegisterAddress = RegisterAddress {_unRegisterAddress :: Word16} deriving (Eq,ExampleDataGet,ExampleDataPut,Ord,Show)

newtype RegisterQuantity = RegisterQuantity {_unRegisterQuantity :: Word16} deriving (Eq,ExampleDataGet,ExampleDataPut,Ord,Show)

newtype StartingAddress = StartingAddress {_unStartingAddress :: Word16} deriving (Eq,ExampleDataGet,ExampleDataPut,Ord,Show)

newtype TargetAddress = TargetAddress {_unTargetAddress :: Word16} deriving (Eq,ExampleDataGet,ExampleDataPut,Ord,Show)

--------------------------------------- Serializing Requests  --------------------------------------------


data ByteStringWithIndices e = ByteStringWithIndices { _incomingByteString :: StrictByteString
                                                     , _dataIndices        :: Seq e
                                                     }

-------------------------------------- Internal Package Errors ------------------------------------------------------

data ExampleError idx = ExampleParseError  (Seq idx) String
                      | ExampleFail String
                      | ExampleAbort
                      | ExampleIOError SomeException
                      | ExampleRequestError idx String
                      | ExampleExceptionResponse String
                        deriving (Show)

instance (Show idx,Typeable idx) => Exception (ExampleError idx)

--------------------------------------- AddressType ExampleDataType Instances -------------------------------------------

data LocationType = SimpleLocation Word8          
                 | SimpleLocationExtended Word16 
                 | ComplexLocation Word8           
                 | ComplexLocationExtended Word16  
                   deriving (Eq,Show)

instance ExampleDataPut LocationType where
  exampledataput exampleconfig (SimpleLocation wd8) = exampledataput exampleconfig wd8
  exampledataput exampleconfig (SimpleLocationExtended wd16) = exampledataput exampleconfig wd16
  exampledataput exampleconfig (ComplexLocation wd8) = exampledataput exampleconfig wd8
  exampledataput exampleconfig (ComplexLocationExtended wd16) = exampledataput exampleconfig wd16

validateLocationType :: (Ord idx,Show idx) => LocationType -> ExampleConfiguration idx -> StrictByteString -> Either String StrictByteString
validateLocationType addresstype exampleconfig bs =
  case runGetPartial (getLocationType addresstype exampleconfig) bs of
    Fail _ _  -> Left $ "Failed to parse LocationType.Expected: " ++ show addresstype
    Partial _ -> Left $ "Failed to parse LocationType due to too few bytes. Expected: " ++ show addresstype
    Done parsedaddress bs'
      | parsedaddress == addresstype -> Right bs'
      | otherwise -> Left $ "Expected LocationType: " ++ show addresstype ++ ", but received: " ++ show parsedaddress
                   
                                                     
getLocationType :: (Ord idx,Show idx) => LocationType -> ExampleConfiguration idx -> Get LocationType
getLocationType (SimpleLocation _)          exampleconfig = SimpleLocation <$> exampledataget exampleconfig
getLocationType (SimpleLocationExtended _)  exampleconfig = SimpleLocationExtended <$> exampledataget exampleconfig
getLocationType (ComplexLocation _)         exampleconfig = ComplexLocation <$> exampledataget exampleconfig
getLocationType (ComplexLocationExtended _) exampleconfig = ComplexLocationExtended <$> exampledataget exampleconfig

