-- {{{ type/data definitions
data IPAddress            = IPAddress Int Int Int Int
                           deriving Eq

type Bandwidth            = Integer

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
foldSimpleNetwork ::  (Bandwidth -> [a] -> a) -> 
                      (Bandwidth -> StorageCapacity -> PCOperatingSystem -> a) -> 
                      SimpleNetwork -> a 
foldSimpleNetwork swf scf = f
    where f (SimpleSwitch bw ns)      = swf bw (map f ns)
          f (SimpleComputer bw sc os) = scf bw sc os
      
-- foldNetwork ::
foldNetwork :: (IPAddress -> Bandwidth -> [a] -> a) -> -- sf
               (IPAddress -> Bandwidth -> StorageCapacity -> PCOperatingSystem -> a) ->  -- cf
               (IPAddress -> Bandwidth -> [a] -> a) -> -- af
               (IPAddress -> Bandwidth -> [a] -> a) -> -- rf
               (IPAddress -> Bandwidth -> StorageCapacity -> PCOperatingSystem -> a) -> -- lf
               (IPAddress -> Bandwidth -> PhoneOperatingSystem -> a ) -> -- mf
               (IPAddress -> Bandwidth -> [a] -> a) -> -- bf 
               Network -> a
foldNetwork sf cf af rf lf mf bf = f
    where f (Switch ip bw ns)       = sf ip bw (map f ns) 
          f (Computer ip bw sw os)  = cf ip bw sw os
          f (AccessPoint ip bw wns) = af ip bw (map (foldWirelessNetwork rf lf mf bf sf cf af) wns)

-- foldWirelessNetwork ::
foldWirelessNetwork :: (IPAddress -> Bandwidth -> [a] -> a) -> -- rf
                       (IPAddress -> Bandwidth -> StorageCapacity -> PCOperatingSystem -> a) -> -- lf
                       (IPAddress -> Bandwidth -> PhoneOperatingSystem -> a ) -> -- mf
                       (IPAddress -> Bandwidth -> [a] -> a) -> -- bf 
                       (IPAddress -> Bandwidth -> [a] -> a) -> -- sf
                       (IPAddress -> Bandwidth -> StorageCapacity -> PCOperatingSystem -> a) ->  -- cf
                       (IPAddress -> Bandwidth -> [a] -> a) -> -- af
                       WirelessNetwork -> a
foldWirelessNetwork rf lf mf bf sf cf af = f
    where f (Repeater ip bw wns)   = rf ip bw (map f wns)
          f (Laptop ip bw sc os)   = lf ip bw sc os
          f (MobilePhone ip bw os) = mf ip bw os
          f (Bridge ip bw ns)      = bf ip bw (map (foldNetwork sf cf af rf lf mf bf) ns)

-- define following functions using folds
-- countComputers :: Network -> Int
countComputers :: Network -> Int
countComputers = foldNetwork sf cf af rf lf mf bf
    where sf _ _ cs  = sum cs
          cf _ _ _ _ = 1
          af _ _ cs  = sum cs
          rf _ _ cs  = sum cs
          lf _ _ _ _ = 0
          mf _ _ _   = 0
          bf _ _ cs  = sum cs

-- countPhones :: Network -> Int
countPhones :: Network -> Int
countPhones = foldNetwork sf cf af rf lf mf bf
    where sf _ _ ps  = sum ps
          cf _ _ _ _ = 0
          af _ _ ps  = sum ps
          rf _ _ ps  = sum ps
          lf _ _ _ _ = 0
          mf _ _ _   = 1
          bf _ _ ps  = sum ps

-- maximumCommonBandwidth :: Network -> Bandwidth
maximumCommonBandwidth :: Network -> Bandwidth
maximumCommonBandwidth = foldNetwork sf cf af rf lf mf bf
    where sf _ bw bws = minimum (bw:bws)
          cf _ bw _ _ = bw
          af _ bw bws = minimum (bw:bws)
          rf _ bw bws = minimum (bw:bws)
          lf _ bw _ _ = bw
          mf _ bw _   = bw
          bf _ bw bws = minimum (bw:bws)

-- countPCOperatingSystems :: PCOperatingSystem  -> Network -> Int
countPCOperatingSystems :: PCOperatingSystem -> Network -> Integer
countPCOperatingSystems os = foldNetwork sf cf af rf lf mf bf
    where sf _ _ oss   = sum oss
          cf _ _ _ cos | cos == os = 1
                       | otherwise = 0
          af _ _ oss   = sum oss
          rf _ _ oss   = sum oss
          lf _ _ _ cos | cos == os = 1
                       | otherwise = 0
          mf _ _ _     = 0
          bf _ _ oss   = sum oss

-- hasDuplicateIPAddress :: Network -> Bool
hasDuplicateIPAddress :: Network -> Bool
hasDuplicateIPAddress =  hasDuplicates.allIPAddresses
       -- hasDuplicates :: [a] -> Bool
    where hasDuplicates []     = False
          hasDuplicates (x:xs) = x `elem` xs || hasDuplicates xs
       -- allIPAddresses :: Network -> [IPAddress]
          allIPAddresses = foldNetwork sf cf af rf lf mf bf
          sf ip _ ips = concat ([ip]:ips)
          cf ip _ _ _ = [ip]
          af ip _ ips = concat ([ip]:ips)
          rf ip _ ips = concat ([ip]:ips)
          lf ip _ _ _ = [ip]
          mf ip _ _   = [ip]
          bf ip _ ips = concat ([ip]:ips)

-- mostUsedPhoneOperatingSystem :: Network -> PhoneOperatingSystem
mostUsedPhoneOperatingSystem ns = fst $ head $ sortP [ (o, countPhoneOperatingSystems o ns) | o <- [Android .. Maemo] ] 
    where sortP ((a,b):xs) = sortP [(c,d)|(c,d) <- xs, b<d] ++ [(a,b)] ++ sortP [(c,d)|(c,d) <- xs, b>=d]

countPhoneOperatingSystems :: PhoneOperatingSystem -> Network -> Integer
countPhoneOperatingSystems os = foldNetwork sf cf af rf lf mf bf
    where sf _ _ oss = sum oss
          cf _ _ _ _ = 0
          af _ _ oss = sum oss
          rf _ _ oss = sum oss
          lf _ _ _ _ = 0
          mf _ _ pos | pos == os = 1
                     | otherwise = 0
          bf _ _ oss = sum oss  

-- }}} excersises

-- {{{ testcases

-- }}} testcases
