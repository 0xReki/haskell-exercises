-- {{{ type/data definitions
data IPAddress            = IPAddress Int Int Int Int

type Bandwith             = Integer

type StorageCapacity      = Integer

data PCOperatingSystem    = Linux 
                          | Windows
                          | BSD
                          | MacOS
                           deriving (Enum, Show)

data PhoneOperatingSystem = Android
                          | Symbian
                          | IOS
                          | WindowsPhone
                          | Maemo
                           deriving (Enum, Show)

data SimpleNetwork        = SSwitch       Bandwith [Network]
                          | SComputer     Bandwith StorageCapacity PCOperatingSystem

data Network              = Switch       IPAddress Bandwith [Network]
                          | Computer     IPAddress Bandwith StorageCapacity PCOperatingSystem
                          | AccessPoint  IPAddress Bandwith [WirelessNetwork]

data WirelessNetwork      = Repeater     IPAddress Bandwith [WirelessNetwork]
                          | Laptop       IPAddress Bandwith StorageCapacity PCOperatingSystem
                          | MobilePhone  IPAddress Bandwith PhoneOperatingSystem
                          | Bridge       IPAddress Bandwith [Network]
-- }}} type/data definitions

-- {{{ excersises
-- Define following folds and their types
-- foldSimpleNetwork ::
--

-- foldNetwork ::
--

-- foldWirelessNetwork ::
--

-- define following functions with and without using folds
-- hasDuplicateIPAddress :: Network -> Bool
--
--

-- maximumCommonBandwith :: Network -> Bandwith
--

-- countComputers :: Network -> Int
--

-- countPhones :: Network -> Int
--

-- countPCOperatingSystems :: Network -> PCOperatingSystems -> Int
--

-- mostUsedPhoneOperatingSystem :: Network -> PhoneOperatingSystem
--
-- }}} excersises

-- {{{ testcases

-- }}} testcases
