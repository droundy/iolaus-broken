import qualified Iolaus.Bug as Bug_
import qualified Iolaus.RepoPath as RepoPath_

#define darcsBug (\imp_funny_name -> imp_funny_name (__FILE__,__LINE__,__TIME__,__DATE__))

#define bug        (darcsBug (seq RepoPath_.assertLocal_ Bug_._bug))
#define impossible (darcsBug (seq RepoPath_.assertLocal_ Bug_._impossible))
#define fromJust   (darcsBug (seq RepoPath_.assertLocal_ Bug_._assertJust (const "fromJust error at ") id))
#define assertLocal (darcsBug (Bug_._assertJust (\p__ -> "Path "++show p__++" is not local at ") RepoPath_.assertLocal_))
#define bugDoc     (darcsBug (Bug_._bugDoc))
