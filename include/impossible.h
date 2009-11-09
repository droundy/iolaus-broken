
import qualified Iolaus.Bug as Bug_

#define darcsBug (\imp_funny_name -> imp_funny_name (__FILE__,__LINE__,__TIME__,__DATE__))

#define bug        (darcsBug Bug_._bug)
#define impossible (darcsBug Bug_._impossible)
#define fromJust   (darcsBug (Bug_._assertJust (const "fromJust error at ") id))
#define bugDoc     (darcsBug Bug_._bugDoc)
