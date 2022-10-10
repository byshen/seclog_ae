#ifndef DG_POINTS_TO_SET_H_
#define DG_POINTS_TO_SET_H_

#include "PointsToSets/OffsetsSetPointsToSet.h"
#include "PointsToSets/SimplePointsToSet.h"
#include "PointsToSets/SeparateOffsetsPointsToSet.h"
#include "PointsToSets/PointerIdPointsToSet.h"
#include "PointsToSets/SmallOffsetsPointsToSet.h"
#include "PointsToSets/AlignedSmallOffsetsPointsToSet.h"
#include "PointsToSets/AlignedPointerIdPointsToSet.h"

namespace dg {
namespace pta {

using PointsToSetT = OffsetsSetPointsToSet;
using PointsToMapT = std::map<Offset, PointsToSetT>;

} // namespace pta
} // namespace dg

#endif
