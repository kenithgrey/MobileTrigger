##############################
# Copyright 2019 Kenith Grey #
##############################

# Copyright Notice --------------------------------------------------------
# This file is part of MobileTrigger.
#
# MobileTrigger is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# MobileTrigger is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with MobileTrigger.  If not, see <http://www.gnu.org/licenses/>.

#### Just Helper Functions
 ## Example
    # pathResult <- .pathTest(path)
    # if(pathResult[1] == FALSE){
    #  stop(pathResult[2])
    # }

.pathTest <- function(path = "", message="Please provied a path (e.g., c:/triggers)"){
  if(is.null(path) == TRUE){
    return(list(status=FALSE, message=message))
  }else if (nchar(path) == 0){
    return(list(status=FALSE, message=message))
  }else{
    return(TRUE)
  }
}
