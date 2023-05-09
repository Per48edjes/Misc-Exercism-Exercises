module Clock (addDelta, fromHourMin, toString) where

type Hour = Int
type Minute = Int

data Clock = Clock Hour Minute

instance Eq Clock where
    (==) (Clock h1 m1) (Clock h2 m2) = hour1 == hour2 && minute1 == minute2
      where
        minute1 = m1 `mod` 60
        hour1 = (h1 + (m1 `div` 60)) `mod` 24
        minute2 = m2 `mod` 60
        hour2 = (h2 + (m2 `div` 60)) `mod` 24

fromHourMin :: Int -> Int -> Clock
fromHourMin = Clock

toString :: Clock -> String
toString (Clock h m) = (if hour `mod` 24 < 10 then '0' : show hour else show hour) ++ ":" ++ (if minute < 10 then '0' : show minute else show minute)
  where
    minute = m `mod` 60
    hour = (h + (m `div` 60)) `mod` 24

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour minute (Clock h m) = Clock newHour newMin
  where
    newMin = (m + minute) `mod` 60
    newHour = (h + hour + ((m + minute) `div` 60)) `mod` 24
