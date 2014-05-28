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

-- {{{ case 1
network1 :: Network
network1 = 
    Switch                      (IPAddress 10 0 0 1)    10000           [
        Switch                  (IPAddress 10 0 10 1)   1000            [
            Computer            (IPAddress 10 0 10 10)  1000  1000      Windows,
            Computer            (IPAddress 10 0 10 20)  1000  100       MacOS
        ],
        Switch                  (IPAddress 10 0 20 1)   1000            [
            Computer            (IPAddress 10 0 20 10)  1000  1000      Windows,
            Computer            (IPAddress 10 0 20 10)  1000  100000    Linux, -- duplicate IPAddress here
            Computer            (IPAddress 10 0 20 30)  1000  1000      Windows,
            Computer            (IPAddress 10 0 20 40)  1000  100       BSD
        ]
    ] 

network1' :: Network
network1' = 
    Switch                      (IPAddress 10 0 0 1)    10000           [
        Switch                  (IPAddress 10 0 10 1)   1000            [
            Computer            (IPAddress 10 0 10 10)  1000  1000      Windows,
            Computer            (IPAddress 10 0 10 20)  1000  100       MacOS
        ],
        Switch                  (IPAddress 10 0 20 1)   1000            [
            Computer            (IPAddress 10 0 20 10)  1000  1000      Windows,
            Computer            (IPAddress 10 0 20 15)  1000  100000    Linux, -- duplicate IPAddress here
            Computer            (IPAddress 10 0 20 30)  1000  1000      Windows,
            Computer            (IPAddress 10 0 20 40)  1000  100       BSD
        ]
    ] 
-- }}} case 1

-- {{{ case 2
network2 :: Network
network2 =
    Switch                      (IPAddress 10 0 0 10)   10000           [
        Switch                  (IPAddress 10 10 10 1)  10000           [
            Computer            (IPAddress 10 10 10 10) 1000  1000000   Windows,
            Computer            (IPAddress 10 10 10 20) 100   1000000   Linux
        ],
        ap1
    ]

network2' :: Network
network2' =
    Switch                      (IPAddress 10 0 0 10)   10000           [
        Switch                  (IPAddress 10 10 10 1)  10000           [
            Computer            (IPAddress 10 10 10 10) 1000  1000000   Windows,
            Computer            (IPAddress 10 10 10 20) 100   1000000   Linux
        ]
    ]
   
ap1 :: Network
ap1 =
    AccessPoint             (IPAddress 10 10 20 1)  500             [
        MobilePhone         (IPAddress 10 10 20 10) 100             Android,
        MobilePhone         (IPAddress 10 10 20 20) 10              Symbian,
        MobilePhone         (IPAddress 10 10 20 30) 100             Android,
        MobilePhone         (IPAddress 10 10 20 40) 100             Android,
        MobilePhone         (IPAddress 10 10 20 50) 100             IOS,
        MobilePhone         (IPAddress 10 10 20 60) 100             Android,
        Laptop              (IPAddress 10 10 10 10) 1000 10000      MacOS -- duplicate IPAddress here
    ] 
-- }}} case 2

-- {{{ case 3
network :: Network
network =
    Switch                  (IPAddress 10 0 0 0)    100000               [
        network1,
        network1',
        network2,
        network2'
    ]
-- }}} case 3

-- }}} testcases
