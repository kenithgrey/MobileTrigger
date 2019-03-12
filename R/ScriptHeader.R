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


#' @export
#' @title Header Template for Script Files
#' @description Quick printout of an XML style header for script files.
#' @return Template Header for Scripts
#'
#' @examples
#'
#' #ScriptHeader()

ScriptHeader <- function(){
return(cat(
"
# <SCRIPT>
# <Title> Script Title </Title>
# <Description> Script Description </Description>
# <Inputs> List of Inputs </Inputs>
# </SCRIPT>
"
))
}
