-- {{{ type/data definitions
data IPAddress            = IPAddress Int Int Int Int
                           deriving Eq

type Bandwidth             = Integer

type StorageCapacity      = Integer

data PCOperatingSystem    = Linux 
                          | Windows
                          | BSD
                          | MacOS
                           deriving (Enum, Show, Eq)

data PhoneOperatingSystem = Android
                          | Symbian
                          | IOS
                          | WindowsPhone
                          | Maemo
                           deriving (Enum, Show, Eq)

data SimpleNetwork        = SimpleSwitch       Bandwidth [SimpleNetwork]
                          | SimpleComputer     Bandwidth StorageCapacity PCOperatingSystem

data Network              = Switch       IPAddress Bandwidth [Network]
                          | Computer     IPAddress Bandwidth StorageCapacity PCOperatingSystem
                          | AccessPoint  IPAddress Bandwidth [WirelessNetwork]

data WirelessNetwork      = Repeater     IPAddress Bandwidth [WirelessNetwork]
                          | Laptop       IPAddress Bandwidth StorageCapacity PCOperatingSystem
                          | MobilePhone  IPAddress Bandwidth PhoneOperatingSystem
                          | Bridge       IPAddress Bandwidth [Network]
-- }}} type/data definitions

-- {{{ excersises
-- Define following folds and their types
-- foldSimpleNetwork ::
      
-- foldNetwork ::

-- foldWirelessNetwork ::

-- define following functions using folds
-- countComputers :: Network -> Int

-- countPhones :: Network -> Int

-- maximumCommonBandwidth :: Network -> Bandwidth

-- countPCOperatingSystems :: PCOperatingSystem  -> Network -> Int

-- hasDuplicateIPAddress :: Network -> Bool

-- mostUsedPhoneOperatingSystem :: Network -> PhoneOperatingSystem

-- }}} excersises

-- {{{ testcases

-- }}} testcases
