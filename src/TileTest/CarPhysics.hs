{-# LANGUAGE DeriveGeneric #-}
module TileTest.CarPhysics(
    carSimulationStep
  , CarType(..)
  , Car(..)
  , defaultCar
  ) where

import           Control.Applicative ((<$>))
import           Control.Lens.Getter ((^.))
import           Data.Bool           (not, (&&))
import           Data.Default        (Default, def)
import           Data.Eq             ((==))
import           Data.Function       (($), (.))
import           Data.List           (elem)
import           Data.Ord            (min, (<), (>=))
import           Data.Ord            (Ord)
import           GHC.Generics        (Generic)
import           Linear.V2           (V2 (..), _x, _y)
import           Linear.Vector       ((*^), (^+^), (^/))
import           Prelude             (Eq, abs, atan2, cos, id, max, pi, signum,
                                      sin, (*), (+), (-), (/))
import           Text.Show           (Show)
import           Wrench.FloatType
import           Wrench.Point
import           Wrench.Time

data CarAction = CarBrake | CarLeft | CarRight | CarForward deriving (Eq)

clamp :: Ord a => a -> a -> a -> a
clamp low high v = max low (min high v)

gcThrottle :: FloatType
gcThrottle = 50

gcDrag :: FloatType
gcDrag = 5

gcRollingResistance :: FloatType
gcRollingResistance = 150

gcCorneringStiffnessRear :: FloatType
gcCorneringStiffnessRear = -5.2

gcMaxThrottle :: FloatType
gcMaxThrottle = 100

gcMaxBrake :: FloatType
gcMaxBrake = 400

gcCorneringStiffnessFront :: FloatType
gcCorneringStiffnessFront = -5

gcMaxGrip :: FloatType
gcMaxGrip = 2

gcMaxSteerAngle :: FloatType
gcMaxSteerAngle = pi / 4

gcSteerAngle :: FloatType
gcSteerAngle = 4

data CarType = CarType {
    carTypeWheelBase         :: !FloatType
  , carTypeDistanceCgToFront :: !FloatType
  , carTypeDistanceCgToRear  :: !FloatType
  , carTypeHeight            :: !FloatType
  , carTypeMass              :: !FloatType
  , carTypeInertia           :: !FloatType
  , carTypeLength            :: !FloatType
  , carTypeWidth             :: !FloatType
  , carTypeWheelLength       :: !FloatType
  , carTypeWheelWidth        :: !FloatType
  } deriving(Show,Generic)

instance Default CarType where
  def = CarType {
      carTypeDistanceCgToFront = 1.0
    , carTypeDistanceCgToRear = 1.0
    , carTypeWheelBase = 2.0
    , carTypeHeight = 1.0
    , carTypeMass = 1500.0
    , carTypeInertia = 1500.0
    , carTypeWidth = 1.5
    , carTypeLength = 3.0
    , carTypeWheelLength = 0.7
    , carTypeWheelWidth = 0.3
    }

data Car = Car {
    carType            :: !CarType
  , carPosition        :: !Point
  , carVelocity        :: !Point
  , carAngle           :: !FloatType
  , carAngularVelocity :: !FloatType
  , carAcceleration    :: !Point
  , carThrottle        :: !FloatType
  , carBrake           :: !FloatType
  , carSteerAngle      :: !FloatType
  } deriving(Show,Generic)

instance Default Car where
  def = Car {
      carType = def
    , carPosition = V2 0 0
    , carVelocity = V2 0 0
    , carAngle = 0
    , carAngularVelocity = 0
    , carAcceleration = V2 0 0
    , carThrottle = 0
    , carBrake = 0
    , carSteerAngle = 0
    }

defaultCar :: Point -> Car
defaultCar p = Car {
      carType = def
    , carPosition = p
    , carVelocity = V2 0 0
    , carAngle = 0
    , carAngularVelocity = 0
    , carAcceleration = V2 0 0
    , carThrottle = 0
    , carBrake = 0
    , carSteerAngle = 0
    }

traceShowIdPrefix p = case p of
  --"resistance" -> DB.traceShowIdPrefix "resistance"
  {-
                           "flatf" -> DB.traceShowIdPrefix "flatf"
                           "flatr" -> DB.traceShowIdPrefix "flatr"
                           "torque" -> DB.traceShowIdPrefix "torque"
                           "angularAcceleration" -> DB.traceShowIdPrefix "angularAcceleration"
-}
  _ -> id

-- x is to the front of the car, y is to the right, z is down
-- Auto zeigt nach rechts im Grundzustand?
carSimulationStep :: TimeDelta -> [CarAction] -> Car -> Car
carSimulationStep delta ias c =
  let sn = sin (carAngle c)
      cs = cos (carAngle c)
      -- Wenn Winkel des Autos 0: velocity=(vy,vx)
      -- Das Auto fährt mit y-Geschwindigkeit nach rechts und mit x-Geschwindigkeit nach oben
      -- Das ist (Rotationsmatrix um carAngle) * (y,x) (Achtung, Koordinaten vertauscht)
      velocity = traceShowIdPrefix "velocity" $ V2
                   (cs * ((carVelocity c) ^. _y) + sn * ((carVelocity c) ^. _x))
                   (-sn * ((carVelocity c) ^. _y) + cs * ((carVelocity c) ^. _x))
      -- Resulting velocity of the wheels as result of the yaw rate of the car body
      -- v = yawrate * r where r is distance of wheel to CG (approx. half wheel base)
      -- yawrate (ang.velocity) must be in rad/s
      -- wheelBase ist der Abstand der Vorder- und Hinterreifen
      {-
            If the car is turning round the centre of geometry (CG) at a
            rate omega (in rad/s!), this means the front wheels are describing a
            circular path around CG at that same rate. If the car turns full
            circle, the front wheel describes a circular path of distance 2.pi.b
            around CG in 1/(2.pi.omega) seconds where b is the distance from the
            front axle to the CG. This means a lateral velocity of omega * b. For
            the rear wheels, this is -omega * c. Note the sign reversal. To
            express this as an angle, take the arctangent of the lateral velocity
            divided by the longtitudinal velocity (just like we did for beta).
            For small angles we can approximate arctan(vy/vx) by vx/vy.
      -}
      -- Lateral velocity (laut oben): omega * b, genau das was hier steht
      yawspeed = traceShowIdPrefix "yawspeed" $ (carTypeWheelBase . carType $ c) * 0.5 * carAngularVelocity c
      epsilon = 0.0001
      rotAngle = traceShowIdPrefix "rotAngle" $ if (velocity ^. _x) < epsilon
                 then 0
                 else atan2 yawspeed (velocity ^. _x)
      -- Calculate the side slip angle of the car (a.k.a. beta)
      -- Winkelunterschied zwischen Ausrichtung des Autos und Geschwindigkeitsvektor
      sideslip = traceShowIdPrefix "sideslip" $ if (velocity ^. _x) < epsilon
                 then 0
                 else atan2 (velocity ^. _y) (velocity ^. _x)
      {-
      steerAngle = traceShowIdPrefix "steerAngle" $ if (PlayerLeft `elem` ias) == (PlayerRight `elem` ias)
                   then 0
                   else if PlayerLeft `elem` ias
                        then -steerAngleAbsolute
                        else steerAngleAbsolute
-}
      -- Calculate slip angles for front and rear wheels (a.k.a. alpha)
      -- Winkelunterschied zwischen Geschwindigkeitsvektor des Autos und Richtung der Reifen
      slipanglefront = traceShowIdPrefix "slipanglefront" $ sideslip + rotAngle - (carSteerAngle c)
      -- Dasselbe für Hinterreifen. Die haben aber natürlich keinen steerAngle.
      slipanglerear = traceShowIdPrefix "slipanglerear" $ sideslip - rotAngle
      frontslip = CarBrake `elem` ias
      rearslip = CarBrake `elem` ias
      weight = traceShowIdPrefix "weight" $ (carTypeMass . carType $ c) * 9.8 * 0.5
      -- lateral force on front wheels = (Ca * slip angle) capped to friction circle * load
      flatf = traceShowIdPrefix "flatf" $ V2
                0
                ((if frontslip then 0.5 else 1.0) * (weight * (clamp (-gcMaxGrip) gcMaxGrip $ gcCorneringStiffnessFront * slipanglefront)))
      flatr = traceShowIdPrefix "flatr" $ V2
                0
                ((if rearslip  then 0.5 else 1.0) * (weight * (clamp (-gcMaxGrip) gcMaxGrip $ gcCorneringStiffnessRear * slipanglerear)))
      -- longtitudinal force on rear wheels - very simple traction model
      ftraction = traceShowIdPrefix "ftraction" $ V2
                    ((if rearslip then 0.5 else 1.0) * 100.0 * (carThrottle c - carBrake c * (if velocity ^. _x >= 0 then 1 else -1)))
                    0
      -- drag and rolling resistance
      resistance = traceShowIdPrefix "resistance" $ (-1) *^ (gcRollingResistance *^ velocity + gcDrag *^ velocity * (abs <$> velocity))
      -- sum forces: F=m*a
      force = traceShowIdPrefix "force" $ ftraction ^+^ (V2 (sin (carSteerAngle c)) (cos (carSteerAngle c))) * flatf ^+^ flatr ^+^ resistance
      -- torque on body from lateral forces
      torque = traceShowIdPrefix "torque" $ (carTypeDistanceCgToFront . carType $ c) * (flatf ^. _y) - (carTypeDistanceCgToRear . carType $ c) * (flatr ^. _y)
      -- F=m*a / m => a
      acceleration = traceShowIdPrefix "acceleration" $ force ^/ (carTypeMass . carType $ c)
      angularAcceleration = traceShowIdPrefix "angularAcceleration" $ torque / (carTypeInertia . carType $ c)
      -- Hier wird von "x = nach vorne, y = zur Seite" umgewechselt
      accelerationWc = traceShowIdPrefix "accelerationWc" $ V2 (cs * (acceleration ^. _y) + sn * (acceleration ^. _x)) (-sn * (acceleration ^. _y) + cs * (acceleration ^. _x))
      d = toSeconds delta
      velocityWc = traceShowIdPrefix "velocityWc" $ carVelocity c ^+^ d *^ accelerationWc
      angularVelocity = traceShowIdPrefix "angularVelocity" $ (carAngularVelocity c) + d * angularAcceleration
      steerDirection = if (CarLeft `elem` ias) == (CarRight `elem` ias)
                       then ((-1) * signum (carSteerAngle c))
                       else if CarLeft `elem` ias then -1 else 1
  in c {
      carVelocity = velocityWc
    , carPosition = carPosition c ^+^ d *^ velocityWc
    , carAcceleration = accelerationWc
    , carAngularVelocity = angularVelocity
    , carAngle = carAngle c + d * angularVelocity
    , carThrottle = if CarForward `elem` ias && (not (CarBrake `elem` ias))
                    then min gcMaxThrottle (carThrottle c + d * gcThrottle)
                    else 0
    , carSteerAngle = clamp (-gcMaxSteerAngle) gcMaxSteerAngle $ carSteerAngle c + steerDirection * d * gcSteerAngle
    , carBrake = if CarBrake `elem` ias then gcMaxBrake else 0
    {-, carThrottle = if CarBrake `elem` ias
                    then 0
                    else if CarForward `elem` ias
                         then min gcMaxThrottle (carThrottle c + gcThrottle * d)
                         else 0-]
    , carBrake = if CarBrake `elem` ias then gcMaxThrottle else 0-}
    }
