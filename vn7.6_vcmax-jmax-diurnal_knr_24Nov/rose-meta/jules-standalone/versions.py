import re
import sys
if sys.version_info[0] == 2:
    from rose.upgrade import MacroUpgrade
else:
    from metomi.rose.upgrade import MacroUpgrade

from .version34_40 import *
from .version40_41 import *
from .version41_42 import *
from .version42_43 import *
from .version43_44 import *
from .version44_45 import *
from .version45_46 import *
from .version46_47 import *
from .version47_48 import *
from .version48_49 import *
from .version49_50 import *
from .version50_51 import *
from .version51_52 import *
from .version52_53 import *
from .version53_54 import *
from .version54_55 import *
from .version55_56 import *
from .version56_57 import *
from .version57_58 import *
from .version58_59 import *
from .version59_60 import *
from .version60_61 import *
from .version61_62 import *
from .version62_63 import *
from .version63_70 import *
from .version70_71 import *
from .version71_72 import *
from .version72_73 import *
from .version73_74 import *
from .version74_75 import *
from .version75_76 import *


class vn76_t1573(MacroUpgrade):

    """Upgrade macro from JULES by Phil Harris"""

    BEFORE_TAG = "vn7.6"
    AFTER_TAG = "vn7.6_t1573"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings.  Default tdq10 parameters are from Tjoelker et al
        # (2001) doi:10.1046/j.1365-2486.2001.00397.x.
        self.add_setting(config, ["namelist:jules_vegetation", "rd_model"], "1")
        self.add_setting(config, ["namelist:jules_vegetation", "tdq10_slope"], "-0.046")
        self.add_setting(config, ["namelist:jules_vegetation", "tdq10_int"], "3.22")
        return config, self.reports


