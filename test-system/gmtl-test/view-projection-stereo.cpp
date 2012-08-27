

//==========================================================================nix
/**
 * @file   test.cpp
 * @author Nix <nix@naunine.com>
 * @date   Sun May  9 22:13:04 2010
 *
 * @brief
 */
//==========================================================================nix

#include <gmtl/Vec.h>
#include <gmtl/Matrix.h>
#include <gmtl/MatrixOps.h>
#include <gmtl/Generate.h>
#include <gmtl/Output.h>
#include <gmtl/Xforms.h>
#include <gmtl/Point.h>

using namespace gmtl;
using namespace std;

#define N11 1 //left-screen //left-eye
//#define N12 1 //right-screen //left-eye
//#define N13 1   //left-screen //right-eye
//#define N14 1 //right-screen //right-eye

#ifdef N11
#define LEFT_EYE 1
#endif

#ifdef N12
#define LEFT_EYE 1
#endif

#ifdef N13
#define RIGHT_EYE 1
#endif

#ifdef N14
#define RIGHT_EYE 1
#endif


gmtl::Matrix44f cur_head_pos;
float scaleFactor = 1.0f;
float mNearDist = 0.1f ;
float mFarDist = 10000.0f;


//--------------------------------------------------------------------------nix
/**
 * MAIN This is where is all starts
 *
 * @param argc Total number of input parameters
 * @param argv Array of input strings
 *
 * @return 0 on success and negative numbers on error
 */

int main(int argc, char *argv[])
{
  gmtl::Vec3f head(0.0,1.8,0.0);
  gmtl::setTrans(cur_head_pos,head);
  //node 11
  #ifdef N11
  gmtl::Point3f mLLCorner(-1.525,1.08,-3.2);
  gmtl::Point3f mLRCorner(0.201,1.08,-3.2);
  gmtl::Point3f mURCorner(0.201,2.38,-3.2);
  gmtl::Point3f mULCorner(-1.525,2.38,-3.2);
  #endif
  #ifdef N12
  //node 12
  gmtl::Point3f mLLCorner(-0.201,1.08,-3.2);
  gmtl::Point3f mLRCorner(1.525,1.08,-3.2);
  gmtl::Point3f mURCorner(1.525,2.38,-3.2);
  gmtl::Point3f mULCorner(-0.201,2.38,-3.2);
  #endif
  #ifdef N13
  //node 13
  gmtl::Point3f mLLCorner(-1.524,1.08,-3.2);
  gmtl::Point3f mLRCorner(0.202,1.08,-3.2);
  gmtl::Point3f mURCorner(0.202,2.38,-3.2);
  gmtl::Point3f mULCorner(-1.524,2.38,-3.2);
  #endif
  #ifdef N14
  //node 14
  gmtl::Point3f mLLCorner(-0.202,1.08,-3.2);
  gmtl::Point3f mLRCorner(1.524,1.08,-3.2);
  gmtl::Point3f mURCorner(1.524,2.38,-3.2);
  gmtl::Point3f mULCorner(-0.202,2.38,-3.2);
  #endif

  // cout<<"a =" <<mLLCorner<<endl;
  // cout<<"b =" <<mLRCorner<<endl;
  // cout<<"c =" <<mURCorner<<endl;
  // cout<<"d =" <<mULCorner<<endl;

  // // Calculate surface rotation
  // //---------------------------------------------------------------------
  // gmtl::Matrix44f mSurfaceRotation;	/**< Same as m_base_M_surface */

  // // Find the base vectors for the surface axis (in terms of the base coord
  // // system) with z out, x to the right, and y up.
  // gmtl::Vec3f x_base(mLRCorner - mLLCorner);
  // gmtl::Vec3f y_base(mURCorner - mLRCorner);
  // gmtl::Vec3f z_base;
  // gmtl::cross(z_base, x_base, y_base);

  // // They must be normalized.
  // gmtl::normalize(x_base);
  // gmtl::normalize(y_base);
  // gmtl::normalize(z_base);

  // // Calculate the surfaceRotMat using law of cosines.
  // mSurfaceRotation = gmtl::makeDirCos<gmtl::Matrix44f>(x_base, y_base,z_base);
  // cout <<"Surface Rotation = \n" << mSurfaceRotation <<endl;

  // // Calculate base->surface and inverse
  // //---------------------------------------------------------------------
  // gmtl::Matrix44f   m_base_M_surface;
  // gmtl::Matrix44f   m_surface_M_base;  /**< Stored inverse for performance */
  // m_base_M_surface = mSurfaceRotation;
  // gmtl::invert(m_surface_M_base, m_base_M_surface);

  // Corners in base frame
  //----------------------------------------------------------------------
  gmtl::Point3f mxLLCorner;
  gmtl::Point3f mxLRCorner;
  gmtl::Point3f mxURCorner;
  gmtl::Point3f mxULCorner;
  // mxLLCorner = m_surface_M_base * mLLCorner;
  // mxLRCorner = m_surface_M_base * mLRCorner;
  // mxURCorner = m_surface_M_base * mURCorner;
  // mxULCorner = m_surface_M_base * mULCorner;

  mxLLCorner =  mLLCorner;
  mxLRCorner =  mLRCorner;
  mxURCorner =  mURCorner;
  mxULCorner =  mULCorner;

  // cout<<"a =" <<mxLLCorner<<endl;
  // cout<<"b =" <<mxLRCorner<<endl;
  // cout<<"c =" <<mxURCorner<<endl;
  // cout<<"d =" <<mxULCorner<<endl;

  //Calculate Offsets
  //----------------------------------------------------------------------
  float mOriginToScreen;
  float mOriginToRight;
  float mOriginToLeft;
  float mOriginToTop;
  float mOriginToBottom;

  mOriginToScreen = -mxLLCorner[gmtl::Zelt];
  mOriginToRight  = mxLRCorner[gmtl::Xelt];
  mOriginToLeft   = -mxLLCorner[gmtl::Xelt];
  mOriginToTop    = mxURCorner[gmtl::Yelt];
  mOriginToBottom = -mxLRCorner[gmtl::Yelt];


  // Eye specific position
  //--------------------------------------------
  gmtl::Matrix44f eyePos;

  float interocular_dist = 0.064f;
  interocular_dist *= scaleFactor;              // Scale eye separation
  float eye_offset = interocular_dist/2.0f;      // Distance to move eye


#ifdef LEFT_EYE
   eyePos = cur_head_pos * gmtl::makeTrans<gmtl::Matrix44f>(gmtl::Vec3f( -eye_offset, 0, 0));
#endif
#ifdef RIGHT_EYE
   eyePos = cur_head_pos * gmtl::makeTrans<gmtl::Matrix44f>(gmtl::Vec3f(eye_offset, 0, 0));
#endif

  // Frustum Calculations
  //----------------------------------------------------------------------
  const float near = mNearDist;
  const float far = mFarDist;

   // Distance measurements from eye to screen/edges
   // Distance to edges is from the point on the screen plane
   // where a normal line would go through the origin.
   float eye_to_screen, eye_to_right, eye_to_left, eye_to_top, eye_to_bottom;

   // Compute transformed eye position
   // - Converts eye coords into the surface's coord system
   // Xformed position of eyes
   const gmtl::Point3f eye_surface =
      /*m_surface_M_base **/ gmtl::makeTrans<gmtl::Point3f>(eyePos);

   cout << "Eye Surface =" << eye_surface <<endl;
   // Compute dist from eye to screen/edges
   // Take into account scale since all origin to anythings are in meters
   eye_to_screen = (scaleFactor * mOriginToScreen) + eye_surface[gmtl::Zelt];
   eye_to_right  = (scaleFactor * mOriginToRight) - eye_surface[gmtl::Xelt];
   eye_to_left   = (scaleFactor * mOriginToLeft) + eye_surface[gmtl::Xelt];
   eye_to_top    = (scaleFactor * mOriginToTop) - eye_surface[gmtl::Yelt];
   eye_to_bottom = (scaleFactor * mOriginToBottom) + eye_surface[gmtl::Yelt];

   // Distances in near plane, in near plane from origin.  (Similar to above)
   // Find dists on near plane using similar triangles
   const float near_dist_front = near / eye_to_screen; // constant factor
   const float left   = -(eye_to_left * near_dist_front);
   const float right  = eye_to_right * near_dist_front;
   const float top    = eye_to_top * near_dist_front;
   const float bottom = -(eye_to_bottom * near_dist_front);

   // Set frustum and calulcate the matrix.
//   glFrustum(left, right, bottom,top, near, far);
  cout << "Projection Matrix ="<<endl
       <<" (set-projection "
       << left << " "
       << right <<" "
       << bottom <<" "
       << top << " "
       << near <<" "
       << far << ")"<<endl;

  // Focus Plane dist needed for drawing
  float mFocusPlaneDist = eye_to_screen;    // Needed for drawing (simulated mode)
  cout << "Focus Plane dist ="<< mFocusPlaneDist <<endl;

  // View Matrix
  //-------------------------------------------------------------------------
  gmtl::Matrix44f   mViewMat;
  // Non-transformed position.
  const gmtl::Vec3f eye_pos(gmtl::makeTrans<gmtl::Vec3f>(eyePos));
  // Need to post translate to get the view matrix at the position of the eye.
  mViewMat = m_surface_M_base * gmtl::makeTrans<gmtl::Matrix44f>(-eye_pos);
  cout << "View Matrix = \n" << mViewMat <<endl;
  return 0;
}

