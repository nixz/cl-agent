#===========================================================================nix
# @file CMakeList.txt
# @author Nikhil Shetty <nikhil.j.shetty@gmail.com>
# @date Tue Mar 30 01:33:24 2010
#
# @brief To build the paraview/vrjuggler client.
#===========================================================================nix

cmake_minimum_required(VERSION 2.6)

PROJECT(test)

#---------------------------------------------------------------------functions
# find_libs
#   in: list of library names
#   out:list containing absolute path for each lib
function(find_libs liblist libpath outlist)
  foreach(libname ${liblist})
    find_library(LIBPATH_${libname} NAMES ${libname} PATHS ${libpath})
    if(NOT LIBPATH_${libname})
      message (SEND_ERROR "Library ${libname} not found.")
    else(NOT LIBPATH_${libname})
      list(APPEND loc_outlist ${LIBPATH_${libname}})
    endif(NOT LIBPATH_${libname})
  endforeach(libname)

  # message (STATUS "out list: ${loc_outlist}")
  set(${outlist} ${loc_outlist} PARENT_SCOPE)
endfunction(find_libs)

# generate installation rules
function(gen_install_rules target)
  install(TARGETS ${target}
    RUNTIME DESTINATION usr/bin
    LIBRARY DESTINATION usr/lib
    ARCHIVE DESTINATION usr/lib/static)
endfunction(gen_install_rules)

#-------------------------------------------------------------------------basic
set(CMAKE_BUILD_TYPE  "RelWithDebInfo")
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/")
FILE(GLOB SRCFILES "*.cpp")


SET(DEP_INCLUDE_DIRS)
SET(DEP_LIBRARY_DIRS)
SET(DEP_LIBRARIES)


INCLUDE_DIRECTORIES("${${PROJECT_NAME}_SOURCE_DIR}/include"
  ${DEP_INCLUDE_DIRS})
#---------------------------------------------------------------------vrjuggler
find_package(Flagpoll REQUIRED)
flagpoll_check_modules(VPR REQUIRED vpr)
flagpoll_check_modules(GMTL REQUIRED gmtl)
flagpoll_check_modules(GADGET REQUIRED gadgeteer)
flagpoll_check_modules(VRJuggler REQUIRED vrjuggler)
flagpoll_check_modules(VRJugglerOGL REQUIRED vrjuggler-opengl)

find_libs("${VPR_LIBRARIES}"          "${VPR_LIBRARY_DIRS}"       VPR_libs)
find_libs("${GADGET_LIBRARIES}"       "${GADGET_LIBRARY_DIRS}"    GADGET_libs)
find_libs("${VRJuggler_LIBRARIES}"    "${VRJuggler_LIBRARY_DIRS}" VRJUGGLER_libs)
find_libs("${VRJugglerOGL_LIBRARIES}" "${VRJugglerOGL_LIBRARY_DIRS}" VRJUGGLEROGL_libs)

LIST(APPEND DEP_INCLUDE_DIRS ${VPR_INCLUDE_DIRS})
LIST(APPEND DEP_INCLUDE_DIRS ${GMTL_INCLUDE_DIRS})
LIST(APPEND DEP_INCLUDE_DIRS ${GADGET_INCLUDE_DIRS})
LIST(APPEND DEP_INCLUDE_DIRS ${VRJuggler_INCLUDE_DIRS})
LIST(APPEND DEP_INCLUDE_DIRS ${VRJugglerOGL_INCLUDE_DIRS})

LIST(APPEND DEP_LIBRARIES ${VPR_libs})
LIST(APPEND DEP_LIBRARIES ${GADGET_libs})
LIST(APPEND DEP_LIBRARIES ${VRJUGGLER_libs})
LIST(APPEND DEP_LIBRARIES ${VRJUGGLEROGL_libs})

#----------------------------------------------------------------------Paraview
#FIND_PACKAGE(ParaView REQUIRED)


#----------------------------------------------------------------------Boost
SET(Boost_USE_MULTITHREAD ON)
SET(Boost_STATIC_LIBS OFF)

FIND_PACKAGE(Boost 1.37.0 REQUIRED COMPONENTS filesystem signals program_options)

IF(Boost_FOUND)
    LIST(APPEND DEP_INCLUDE_DIRS ${Boost_INCLUDE_DIRS})
    LIST(APPEND DEP_LIBRARIES ${Boost_LIBRARIES})
ENDIF(Boost_FOUND)

# #-------------------------------------------------------------------------OpenSG
# SET(OSG_COMPONENTS OSGBase
#                    OSGDrawable
#                    OSGEffectGroups
#                    OSGFileIO
#                    OSGGroup
#                    OSGImageFileIO
#                    OSGState
#                    OSGSystem
#                    OSGUtil
#                    OSGWindow)

# FIND_PACKAGE(OpenSG REQUIRED COMPONENTS ${OSG_COMPONENTS})

# IF(OpenSG_FOUND)
#     LIST(APPEND DEP_INCLUDE_DIRS ${OpenSG_INCLUDE_DIRS})
#     LIST(APPEND DEP_LIBRARIES    ${OpenSG_LIBRARIES})
# ENDIF(OpenSG_FOUND)

#-------------------------------------------------------------------------glut
#FIND_PACKAGE(Glut)
#LIST(APPEND DEP_INCLUDE_DIRS ${GLUT_INCLUDE_DIRS})
LIST(APPEND DEP_LIBRARIES glut)
#FIND_PACKAGE(OpenGL)

#set(CMAKE_CXX_FLAGS "-g -Wall ")

#-------------------------------------------------------------------Application
#INCLUDE(${PARAVIEW_USE_FILE})
INCLUDE_DIRECTORIES("${${PROJECT_NAME}_SOURCE_DIR}"
  ${DEP_INCLUDE_DIRS})

LINK_DIRECTORIES(${DEP_LIBRARY_DIRS})
ADD_EXECUTABLE(test ${SRCFILES})

# TARGET_LINK_LIBRARIES(MenuSystem ${DEP_LIBRARIES})
# INSTALL (TARGETS MenuSystem RUNTIME DESTINATION bin)

# add_executable(MenuSystem
#   AnalogEvent.cpp
#   AnimationManager.cpp
#   AnimationObject.cpp
#   app.cpp
#   Button.cpp
#   ButtonEvent.cpp
#   ButtonEventManager.cpp
#   CheckBox.cpp
#   CWBmath.cpp
#   DefaultEventmanager.cpp
#   DialSlider.cpp
#   DrumMenu.cpp
#   DrumMenuNode.cpp
#   EventBus.cpp
#   Event.cpp
#   EventGlobule.cpp
#   EventHandler.cpp
#   EventManager.cpp
#   FreeType.cpp
#   HeadEventManager.cpp
#   Interaction.cpp
#   IntersectionEvent.cpp
#   ListMenu.cpp
#   ListMenuInteractionManager.cpp
#   ListMenuNode.cpp
#   #main.cpp
#   Menu.cpp
#   MenuEvent.cpp
#   MenuImp.cpp
#   MenuNode.cpp
#   MenuNodeImp.cpp
#   MoveEvent.cpp
#   ObjectGrabber.cpp
#   OneDSlider.cpp
#   OpenGLMenuImp.cpp
#   OpenGLMenuNodeImp.cpp
#   OpenSceneGraphMenuImp.cpp
#   OpenSceneGraphMenuNodeImp.cpp
#   OpenSGCustomNodeBase.cpp
#   OpenSGCustomNode.cpp
#   OpenSGMenuImp.cpp
#   OpenSGMenuNodeImp.cpp
#   OpenSGNav.cpp
#   OpenSGUtil.cpp
#   phantom.cpp
#   PieColorPalette.cpp
#   PieMenu.cpp
#   PieMenuDefaults.cpp
#   PieMenuInteractionManager.cpp
#   PieMenuNode.cpp
#   PieMenuOptions.cpp
#   Publisher.cpp
#   ScaleSlider.cpp
#   SharedState.cpp
#   SharedStateManager.cpp
#   simpleInput.cpp
#   Slider.cpp
#   Subscriber.cpp
#   Timer.cpp
#   TimerEvent.cpp
#   TimerEventManager.cpp
#   TransformSharedState.cpp
#   Util.cpp
#   WandEvent.cpp
#   WandEventManager.cpp
#   )

