+++
title = "Catmull–Clark subdivision surface"
description = ""
date = 2019-10-04T20:43:47Z
aliases = []
[extra]
id = 5279
[taxonomies]
categories = []
tags = []
+++

{{task|3D}}[[Category:Graphics algorithms]]{{requires|Graphics}}
Implement the Catmull-Clark surface subdivision ([[wp:Catmull–Clark_subdivision_surface|description on Wikipedia]]), which is an algorithm that maps from a surface (described as a set of points and a set of polygons with vertices at those points) to another more refined surface. The resulting surface will always consist of a mesh of quadrilaterals.

The process for computing the new locations of the points works as follows when the surface is free of holes:
[[Image:CatmullClark_subdiv_0.png|thumb|Starting cubic mesh; the meshes below are derived from this.]]
[[Image:CatmullClark_subdiv_1.png|thumb|After one round of the Catmull-Clark algorithm applied to a cubic mesh.]]
[[Image:CatmullClark_subdiv_2.png|thumb|After two rounds of the Catmull-Clark algorithm. As can be seen, this is converging to a surface that looks nearly spherical.]]
# for each face, a ''face point'' is created which is the average of all the points of the face.
# for each edge, an ''edge point'' is created which is the average between the center of the edge and the center of the segment made with the face points of the two adjacent faces.
# for each vertex point, its coordinates are updated from (<tt>new_coords</tt>):
## the old coordinates (<tt>old_coords</tt>),
## the average of the face points of the faces the point belongs to (<tt>avg_face_points</tt>),
## the average of the centers of edges the point belongs to (<tt>avg_mid_edges</tt>),
## how many faces a point belongs to (<tt>n</tt>), then use this formula:
      m<sub>1</sub> = (n - 3) / n
      m<sub>2</sub> = 1 / n
      m<sub>3</sub> = 2 / n
      new_coords = (m<sub>1</sub> * old_coords)
                 + (m<sub>2</sub> * avg_face_points)
                 + (m<sub>3</sub> * avg_mid_edges)

Then each face is replaced by new faces made with the new points,
* for a triangle face (a,b,c):
    (a, edge_point<sub>ab</sub>, face_point<sub>abc</sub>, edge_point<sub>ca</sub>)
    (b, edge_point<sub>bc</sub>, face_point<sub>abc</sub>, edge_point<sub>ab</sub>)
    (c, edge_point<sub>ca</sub>, face_point<sub>abc</sub>, edge_point<sub>bc</sub>)
* for a quad face (a,b,c,d):
    (a, edge_point<sub>ab</sub>, face_point<sub>abcd</sub>, edge_point<sub>da</sub>)
    (b, edge_point<sub>bc</sub>, face_point<sub>abcd</sub>, edge_point<sub>ab</sub>)
    (c, edge_point<sub>cd</sub>, face_point<sub>abcd</sub>, edge_point<sub>bc</sub>)
    (d, edge_point<sub>da</sub>, face_point<sub>abcd</sub>, edge_point<sub>cd</sub>)

When there is a hole, we can detect it as follows:
* an edge is the border of a hole if it belongs to only one face,
* a point is on the border of a hole if <tt>n<sub>faces</sub> != n<sub>edges</sub></tt> with <tt>n<sub>faces</sub></tt> the number of faces the point belongs to, and <tt>n<sub>edges</sub></tt> the number of edges a point belongs to.

On the border of a hole the subdivision occurs as follows:
# for the edges that are on the border of a hole, the edge point is just the middle of the edge.
# for the vertex points that are on the border of a hole, the new coordinates are calculated as follows:
## in all the edges the point belongs to, only take in account the middles of the edges that are on the border of the hole
## calculate the average between these points (on the hole boundary) and the old coordinates (also on the hole boundary).

For edges and vertices not next to a hole, the standard algorithm from above is used.


## C

[[file:catmull-C.png|center]]
Only the subdivision part.  The [[Catmull–Clark_subdivision_surface/C|full source]] is way too long to be shown here.  Lots of macros, you'll have to see the full code to know what's what.

```c
vertex face_point(face f)
{
	int i;
	vertex v;

	if (!f->avg) {
		f->avg = vertex_new();
		foreach(i, v, f->v)
			if (!i) f->avg->pos = v->pos;
			else    vadd(f->avg->pos, v->pos);

		vdiv(f->avg->pos, len(f->v));
	}
	return f->avg;
}

#define hole_edge(e) (len(e->f)==1)
vertex edge_point(edge e)
{
	int i;
	face f;

	if (!e->e_pt) {
		e->e_pt = vertex_new();
		e->avg = e->v[0]->pos;
		vadd(e->avg, e->v[1]->pos);
		e->e_pt->pos = e->avg;

		if (!hole_edge(e)) {
			foreach (i, f, e->f)
				vadd(e->e_pt->pos, face_point(f)->pos);
			vdiv(e->e_pt->pos, 4);
		} else
			vdiv(e->e_pt->pos, 2);

		vdiv(e->avg, 2);
	}

	return e->e_pt;
}

#define hole_vertex(v) (len((v)->f) != len((v)->e))
vertex updated_point(vertex v)
{
	int i, n = 0;
	edge e;
	face f;
	coord_t sum = {0, 0, 0};

	if (v->v_new) return v->v_new;

	v->v_new = vertex_new();
	if (hole_vertex(v)) {
		v->v_new->pos = v->pos;
		foreach(i, e, v->e) {
			if (!hole_edge(e)) continue;
			vadd(v->v_new->pos, edge_point(e)->pos);
			n++;
		}
		vdiv(v->v_new->pos, n + 1);
	} else {
		n = len(v->f);
		foreach(i, f, v->f)
			vadd(sum, face_point(f)->pos);
		foreach(i, e, v->e)
			vmadd(sum, edge_point(e)->pos, 2, sum);
		vdiv(sum, n);
		vmadd(sum, v->pos, n - 3, sum);
		vdiv(sum, n);
		v->v_new->pos = sum;
	}

	return v->v_new;
}

model catmull(model m)
{
	int i, j, a, b, c, d;
	face f;
	vertex v, x;

	model nm = model_new();
	foreach (i, f, m->f) {
		foreach(j, v, f->v) {
			_get_idx(a, updated_point(v));
			_get_idx(b, edge_point(elem(f->e, (j + 1) % len(f->e))));
			_get_idx(c, face_point(f));
			_get_idx(d, edge_point(elem(f->e, j)));
			model_add_face(nm, 4, a, b, c, d);
		}
	}
	return nm;
}
```



## Go

{{trans|Python}}


This just prints the new points and faces after 1 iteration of the Catmull-Clark algorithm, agreeing with what the Python results would have been had they been printed rather than plotted.

See the original version for full comments.

```go
package main

import (
    "fmt"
    "sort"
)

type (
    Point [3]float64
    Face  []int

    Edge struct {
        pn1 int   // point number 1
        pn2 int   // point number 2
        fn1 int   // face number 1
        fn2 int   // face number 2
        cp  Point // center point
    }

    PointEx struct {
        p Point
        n int
    }
)

func sumPoint(p1, p2 Point) Point {
    sp := Point{}
    for i := 0; i < 3; i++ {
        sp[i] = p1[i] + p2[i]
    }
    return sp
}

func mulPoint(p Point, m float64) Point {
    mp := Point{}
    for i := 0; i < 3; i++ {
        mp[i] = p[i] * m
    }
    return mp
}

func divPoint(p Point, d float64) Point {
    return mulPoint(p, 1.0/d)
}

func centerPoint(p1, p2 Point) Point {
    return divPoint(sumPoint(p1, p2), 2)
}

func getFacePoints(inputPoints []Point, inputFaces []Face) []Point {
    facePoints := make([]Point, len(inputFaces))
    for i, currFace := range inputFaces {
        facePoint := Point{}
        for _, cpi := range currFace {
            currPoint := inputPoints[cpi]
            facePoint = sumPoint(facePoint, currPoint)
        }
        facePoint = divPoint(facePoint, float64(len(currFace)))
        facePoints[i] = facePoint
    }
    return facePoints
}

func getEdgesFaces(inputPoints []Point, inputFaces []Face) []Edge {
    var edges [][3]int
    for faceNum, face := range inputFaces {
        numPoints := len(face)
        for pointIndex := 0; pointIndex < numPoints; pointIndex++ {
            pointNum1 := face[pointIndex]
            var pointNum2 int
            if pointIndex < numPoints-1 {
                pointNum2 = face[pointIndex+1]
            } else {
                pointNum2 = face[0]
            }
            if pointNum1 > pointNum2 {
                pointNum1, pointNum2 = pointNum2, pointNum1
            }
            edges = append(edges, [3]int{pointNum1, pointNum2, faceNum})
        }
    }
    sort.Slice(edges, func(i, j int) bool {
        if edges[i][0] == edges[j][0] {
            if edges[i][1] == edges[j][1] {
                return edges[i][2] < edges[j][2]
            }
            return edges[i][1] < edges[j][1]
        }
        return edges[i][0] < edges[j][0]
    })
    numEdges := len(edges)
    eIndex := 0
    var mergedEdges [][4]int
    for eIndex < numEdges {
        e1 := edges[eIndex]
        if eIndex < numEdges-1 {
            e2 := edges[eIndex+1]
            if e1[0] == e2[0] && e1[1] == e2[1] {
                mergedEdges = append(mergedEdges, [4]int{e1[0], e1[1], e1[2], e2[2]})
                eIndex += 2
            } else {
                mergedEdges = append(mergedEdges, [4]int{e1[0], e1[1], e1[2], -1})
                eIndex++
            }
        } else {
            mergedEdges = append(mergedEdges, [4]int{e1[0], e1[1], e1[2], -1})
            eIndex++
        }
    }
    var edgesCenters []Edge
    for _, me := range mergedEdges {
        p1 := inputPoints[me[0]]
        p2 := inputPoints[me[1]]
        cp := centerPoint(p1, p2)
        edgesCenters = append(edgesCenters, Edge{me[0], me[1], me[2], me[3], cp})
    }
    return edgesCenters
}

func getEdgePoints(inputPoints []Point, edgesFaces []Edge, facePoints []Point) []Point {
    edgePoints := make([]Point, len(edgesFaces))
    for i, edge := range edgesFaces {
        cp := edge.cp
        fp1 := facePoints[edge.fn1]
        var fp2 Point
        if edge.fn2 == -1 {
            fp2 = fp1
        } else {
            fp2 = facePoints[edge.fn2]
        }
        cfp := centerPoint(fp1, fp2)
        edgePoints[i] = centerPoint(cp, cfp)
    }
    return edgePoints
}

func getAvgFacePoints(inputPoints []Point, inputFaces []Face, facePoints []Point) []Point {
    numPoints := len(inputPoints)
    tempPoints := make([]PointEx, numPoints)
    for faceNum := range inputFaces {
        fp := facePoints[faceNum]
        for _, pointNum := range inputFaces[faceNum] {
            tp := tempPoints[pointNum].p
            tempPoints[pointNum].p = sumPoint(tp, fp)
            tempPoints[pointNum].n++
        }
    }
    avgFacePoints := make([]Point, numPoints)
    for i, tp := range tempPoints {
        avgFacePoints[i] = divPoint(tp.p, float64(tp.n))
    }
    return avgFacePoints
}

func getAvgMidEdges(inputPoints []Point, edgesFaces []Edge) []Point {
    numPoints := len(inputPoints)
    tempPoints := make([]PointEx, numPoints)
    for _, edge := range edgesFaces {
        cp := edge.cp
        for _, pointNum := range []int{edge.pn1, edge.pn2} {
            tp := tempPoints[pointNum].p
            tempPoints[pointNum].p = sumPoint(tp, cp)
            tempPoints[pointNum].n++
        }
    }
    avgMidEdges := make([]Point, len(tempPoints))
    for i, tp := range tempPoints {
        avgMidEdges[i] = divPoint(tp.p, float64(tp.n))
    }
    return avgMidEdges
}

func getPointsFaces(inputPoints []Point, inputFaces []Face) []int {
    numPoints := len(inputPoints)
    pointsFaces := make([]int, numPoints)
    for faceNum := range inputFaces {
        for _, pointNum := range inputFaces[faceNum] {
            pointsFaces[pointNum]++
        }
    }
    return pointsFaces
}

func getNewPoints(inputPoints []Point, pointsFaces []int, avgFacePoints, avgMidEdges []Point) []Point {
    newPoints := make([]Point, len(inputPoints))
    for pointNum := range inputPoints {
        n := float64(pointsFaces[pointNum])
        m1, m2, m3 := (n-3)/n, 1.0/n, 2.0/n
        oldCoords := inputPoints[pointNum]
        p1 := mulPoint(oldCoords, m1)
        afp := avgFacePoints[pointNum]
        p2 := mulPoint(afp, m2)
        ame := avgMidEdges[pointNum]
        p3 := mulPoint(ame, m3)
        p4 := sumPoint(p1, p2)
        newPoints[pointNum] = sumPoint(p4, p3)
    }
    return newPoints
}

func switchNums(pointNums [2]int) [2]int {
    if pointNums[0] < pointNums[1] {
        return pointNums
    }
    return [2]int{pointNums[1], pointNums[0]}
}

func cmcSubdiv(inputPoints []Point, inputFaces []Face) ([]Point, []Face) {
    facePoints := getFacePoints(inputPoints, inputFaces)
    edgesFaces := getEdgesFaces(inputPoints, inputFaces)
    edgePoints := getEdgePoints(inputPoints, edgesFaces, facePoints)
    avgFacePoints := getAvgFacePoints(inputPoints, inputFaces, facePoints)
    avgMidEdges := getAvgMidEdges(inputPoints, edgesFaces)
    pointsFaces := getPointsFaces(inputPoints, inputFaces)
    newPoints := getNewPoints(inputPoints, pointsFaces, avgFacePoints, avgMidEdges)
    var facePointNums []int
    nextPointNum := len(newPoints)
    for _, facePoint := range facePoints {
        newPoints = append(newPoints, facePoint)
        facePointNums = append(facePointNums, nextPointNum)
        nextPointNum++
    }
    edgePointNums := make(map[[2]int]int)
    for edgeNum := range edgesFaces {
        pointNum1 := edgesFaces[edgeNum].pn1
        pointNum2 := edgesFaces[edgeNum].pn2
        edgePoint := edgePoints[edgeNum]
        newPoints = append(newPoints, edgePoint)
        edgePointNums[[2]int{pointNum1, pointNum2}] = nextPointNum
        nextPointNum++
    }
    var newFaces []Face
    for oldFaceNum, oldFace := range inputFaces {
        if len(oldFace) == 4 {
            a, b, c, d := oldFace[0], oldFace[1], oldFace[2], oldFace[3]
            facePointAbcd := facePointNums[oldFaceNum]
            edgePointAb := edgePointNums[switchNums([2]int{a, b})]
            edgePointDa := edgePointNums[switchNums([2]int{d, a})]
            edgePointBc := edgePointNums[switchNums([2]int{b, c})]
            edgePointCd := edgePointNums[switchNums([2]int{c, d})]
            newFaces = append(newFaces, Face{a, edgePointAb, facePointAbcd, edgePointDa})
            newFaces = append(newFaces, Face{b, edgePointBc, facePointAbcd, edgePointAb})
            newFaces = append(newFaces, Face{c, edgePointCd, facePointAbcd, edgePointBc})
            newFaces = append(newFaces, Face{d, edgePointDa, facePointAbcd, edgePointCd})
        }
    }
    return newPoints, newFaces
}

func main() {
    inputPoints := []Point{
        {-1.0, 1.0, 1.0},
        {-1.0, -1.0, 1.0},
        {1.0, -1.0, 1.0},
        {1.0, 1.0, 1.0},
        {1.0, -1.0, -1.0},
        {1.0, 1.0, -1.0},
        {-1.0, -1.0, -1.0},
        {-1.0, 1.0, -1.0},
    }

    inputFaces := []Face{
        {0, 1, 2, 3},
        {3, 2, 4, 5},
        {5, 4, 6, 7},
        {7, 0, 3, 5},
        {7, 6, 1, 0},
        {6, 1, 2, 4},
    }

    outputPoints := make([]Point, len(inputPoints))
    outputFaces := make([]Face, len(inputFaces))
    copy(outputPoints, inputPoints)
    copy(outputFaces, inputFaces)
    iterations := 1
    for i := 0; i < iterations; i++ {
        outputPoints, outputFaces = cmcSubdiv(outputPoints, outputFaces)
    }
    for _, p := range outputPoints {
        fmt.Printf("% .4f\n", p)
    }
    fmt.Println()
    for _, f := range outputFaces {
        fmt.Printf("%2d\n", f)
    }
}
```


{{out}}

```txt

[-0.5556  0.5556  0.5556]
[-0.5556 -0.5556  0.5556]
[ 0.5556 -0.5556  0.5556]
[ 0.5556  0.5556  0.5556]
[ 0.5556 -0.5556 -0.5556]
[ 0.5556  0.5556 -0.5556]
[-0.5556 -0.5556 -0.5556]
[-0.5556  0.5556 -0.5556]
[ 0.0000  0.0000  1.0000]
[ 1.0000  0.0000  0.0000]
[ 0.0000  0.0000 -1.0000]
[ 0.0000  1.0000  0.0000]
[-1.0000  0.0000  0.0000]
[ 0.0000 -1.0000  0.0000]
[-0.7500  0.0000  0.7500]
[ 0.0000  0.7500  0.7500]
[-0.7500  0.7500  0.0000]
[ 0.0000 -0.7500  0.7500]
[-0.7500 -0.7500  0.0000]
[ 0.7500  0.0000  0.7500]
[ 0.7500 -0.7500  0.0000]
[ 0.7500  0.7500  0.0000]
[ 0.7500  0.0000 -0.7500]
[ 0.0000 -0.7500 -0.7500]
[ 0.0000  0.7500 -0.7500]
[-0.7500  0.0000 -0.7500]

[ 0 14  8 15]
[ 1 17  8 14]
[ 2 19  8 17]
[ 3 15  8 19]
[ 3 19  9 21]
[ 2 20  9 19]
[ 4 22  9 20]
[ 5 21  9 22]
[ 5 22 10 24]
[ 4 23 10 22]
[ 6 25 10 23]
[ 7 24 10 25]
[ 7 16 11 24]
[ 0 15 11 16]
[ 3 21 11 15]
[ 5 24 11 21]
[ 7 25 12 16]
[ 6 18 12 25]
[ 1 14 12 18]
[ 0 16 12 14]
[ 6 18 13 23]
[ 1 17 13 18]
[ 2 20 13 17]
[ 4 23 13 20]

```



## Haskell


```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Array
import Data.Foldable (length, concat, sum)
import Data.List (genericLength)
import Data.Maybe (mapMaybe)
import Prelude hiding (length, concat, sum)
import qualified Data.Map.Strict as Map

{-
A SimpleMesh consists of only vertices and faces that refer to them.
A Mesh extends the SimpleMesh to contain edges as well as references to
adjoining mesh components for each other component, such as a vertex
also contains what faces it belongs to.
An isolated edge can be represented as a degenerate face with 2 vertices.
Faces with 0 or 1 vertices can be thrown out, as they do not contribute to
the result (they can also propagate NaNs).
-}

newtype VertexId = VertexId { getVertexId :: Int } deriving (Ix, Ord, Eq, Show)
newtype EdgeId = EdgeId { getEdgeId :: Int } deriving (Ix, Ord, Eq, Show)
newtype FaceId = FaceId { getFaceId :: Int } deriving (Ix, Ord, Eq, Show)

data Vertex a = Vertex
  { vertexPoint :: a
  , vertexEdges :: [EdgeId]
  , vertexFaces :: [FaceId]
  } deriving Show

data Edge = Edge
  { edgeVertexA :: VertexId
  , edgeVertexB :: VertexId
  , edgeFaces :: [FaceId]
  } deriving Show

data Face = Face
  { faceVertices :: [VertexId]
  , faceEdges :: [EdgeId]
  } deriving Show

type VertexArray a = Array VertexId (Vertex a)
type EdgeArray = Array EdgeId Edge
type FaceArray = Array FaceId Face

data Mesh a = Mesh
  { meshVertices :: VertexArray a
  , meshEdges :: EdgeArray
  , meshFaces :: FaceArray
  } deriving Show

data SimpleVertex a = SimpleVertex { sVertexPoint :: a } deriving Show
data SimpleFace = SimpleFace { sFaceVertices :: [VertexId] } deriving Show

type SimpleVertexArray a = Array VertexId (SimpleVertex a)
type SimpleFaceArray = Array FaceId SimpleFace

data SimpleMesh a = SimpleMesh
  { sMeshVertices :: SimpleVertexArray a
  , sMeshFaces :: SimpleFaceArray
  } deriving Show

-- Generic helpers.
fmap1 :: Functor f => (t -> a -> b) -> (t -> f a) -> t -> f b
fmap1 g h x = fmap (g x) (h x)

aZipWith :: Ix i1 => (a -> b -> e) -> Array i1 a -> Array i b -> Array i1 e
aZipWith f a b = listArray (bounds a) $ zipWith f (elems a) (elems b)

average :: (Foldable f, Fractional a) => f a -> a
average xs = (sum xs) / (fromIntegral $ length xs)

-- Intermediary point types for ultimately converting into a point `a`.
newtype FacePoint a = FacePoint { getFacePoint :: a } deriving Show
newtype EdgeCenterPoint a = EdgeCenterPoint { getEdgeCenterPoint :: a } deriving Show
newtype EdgePoint a = EdgePoint { getEdgePoint :: a } deriving Show
newtype VertexPoint a = VertexPoint { getVertexPoint :: a } deriving Show

type FacePointArray a = Array FaceId (FacePoint a)
type EdgePointArray a = Array EdgeId (EdgePoint a)
type EdgeCenterPointArray a = Array EdgeId (EdgeCenterPoint a)
type IsEdgeHoleArray = Array EdgeId Bool
type VertexPointArray a = Array VertexId (VertexPoint a)

-- Subdivision helpers.
facePoint :: Fractional a => Mesh a -> Face -> FacePoint a
facePoint mesh = FacePoint . average . (fmap $ vertexPointById mesh) . faceVertices

allFacePoints :: Fractional a => Mesh a -> FacePointArray a
allFacePoints = fmap1 facePoint meshFaces

vertexPointById :: Mesh a -> VertexId -> a
vertexPointById mesh = vertexPoint . (meshVertices mesh !)

edgeCenterPoint :: Fractional a => Mesh a -> Edge -> EdgeCenterPoint a
edgeCenterPoint mesh (Edge ea eb _)
  = EdgeCenterPoint . average $ fmap (vertexPointById mesh) [ea, eb]

allEdgeCenterPoints :: Fractional a => Mesh a -> EdgeCenterPointArray a
allEdgeCenterPoints = fmap1 edgeCenterPoint meshEdges

allIsEdgeHoles :: Mesh a -> IsEdgeHoleArray
allIsEdgeHoles = fmap ((< 2) . length . edgeFaces) . meshEdges

edgePoint :: Fractional a => Edge -> FacePointArray a -> EdgeCenterPoint a -> EdgePoint a
edgePoint (Edge _ _ [_]) _ (EdgeCenterPoint ecp) = EdgePoint ecp
edgePoint (Edge _ _ faceIds) facePoints (EdgeCenterPoint ecp)
  = EdgePoint $ average [ecp, average $ fmap (getFacePoint . (facePoints !)) faceIds]

allEdgePoints :: Fractional a => Mesh a -> FacePointArray a -> EdgeCenterPointArray a -> EdgePointArray a
allEdgePoints mesh fps ecps = aZipWith (\e ecp -> edgePoint e fps ecp) (meshEdges mesh) ecps

vertexPoint' :: Fractional a => Vertex a -> FacePointArray a -> EdgeCenterPointArray a -> IsEdgeHoleArray -> VertexPoint a
vertexPoint' vertex facePoints ecps iehs
  | length faceIds == length edgeIds = VertexPoint newCoords
  | otherwise = VertexPoint avgHoleEcps
  where
    newCoords = (oldCoords * m1) + (avgFacePoints * m2) + (avgMidEdges * m3)
    oldCoords = vertexPoint vertex
    avgFacePoints = average $ fmap (getFacePoint . (facePoints !)) faceIds
    avgMidEdges = average $ fmap (getEdgeCenterPoint . (ecps !)) edgeIds
    m1 = (n - 3) / n
    m2 = 1 / n
    m3 = 2 / n
    n = genericLength faceIds
    faceIds = vertexFaces vertex
    edgeIds = vertexEdges vertex
    avgHoleEcps = average . (oldCoords:) . fmap (getEdgeCenterPoint . (ecps !)) $ filter (iehs !) edgeIds

allVertexPoints :: Fractional a => Mesh a -> FacePointArray a -> EdgeCenterPointArray a -> IsEdgeHoleArray -> VertexPointArray a
allVertexPoints mesh fps ecps iehs = fmap (\v -> vertexPoint' v fps ecps iehs) (meshVertices mesh)

-- For each vertex in a face, generate a set of new faces from it with its vertex point,
-- neighbor edge points, and face point. The new faces will refer to vertices in the
-- combined vertex array.
newFaces :: Face -> FaceId -> Int -> Int -> [SimpleFace]
newFaces (Face vertexIds edgeIds) faceId epOffset vpOffset
  = take (genericLength vertexIds)
  $ zipWith3 newFace (cycle vertexIds) (cycle edgeIds) (drop 1 (cycle edgeIds))
  where
    f = VertexId . (+ epOffset) . getEdgeId
    newFace vid epA epB = SimpleFace
      [ VertexId . (+ vpOffset) $ getVertexId vid
      , f epA
      , VertexId $ getFaceId faceId
      , f epB]

subdivide :: Fractional a => SimpleMesh a -> SimpleMesh a
subdivide simpleMesh
  = SimpleMesh combinedVertices (listArray (FaceId 0, FaceId (genericLength faces - 1)) faces)
  where
    mesh = makeComplexMesh simpleMesh
    fps = allFacePoints mesh
    ecps = allEdgeCenterPoints mesh
    eps = allEdgePoints mesh fps ecps
    iehs = allIsEdgeHoles mesh
    vps = allVertexPoints mesh fps ecps iehs
    edgePointOffset = length fps
    vertexPointOffset = edgePointOffset + length eps
    combinedVertices
      = listArray (VertexId 0, VertexId (vertexPointOffset + length vps - 1))
      . fmap SimpleVertex
      $ concat [ fmap getFacePoint $ elems fps
               , fmap getEdgePoint $ elems eps
               , fmap getVertexPoint $ elems vps]
    faces
      = concat $ zipWith (\face fid -> newFaces face fid edgePointOffset vertexPointOffset)
      (elems $ meshFaces mesh) (fmap FaceId [0..])

-- Transform to a Mesh by filling in the missing references and generating edges.
-- Faces can be updated with their edges, but must be ordered.
-- Edge and face order does not matter for vertices.
-- TODO: Discard degenerate faces (ones with 0 to 2 vertices/edges),
-- or we could transform these into single edges or vertices.
makeComplexMesh :: forall a. SimpleMesh a -> Mesh a
makeComplexMesh (SimpleMesh sVertices sFaces) = Mesh vertices edges faces
  where
    makeEdgesFromFace :: SimpleFace -> FaceId -> [Edge]
    makeEdgesFromFace (SimpleFace vertexIds) fid
      = take (genericLength vertexIds)
      $ zipWith (\a b -> Edge a b [fid]) verts (drop 1 verts)
      where
        verts = cycle vertexIds

    edgeKey :: VertexId -> VertexId -> (VertexId, VertexId)
    edgeKey a b = (min a b, max a b)

    sFacesList :: [SimpleFace]
    sFacesList = elems sFaces

    fids :: [FaceId]
    fids = fmap FaceId [0..]

    eids :: [EdgeId]
    eids = fmap EdgeId [0..]

    faceEdges :: [[Edge]]
    faceEdges = zipWith makeEdgesFromFace sFacesList fids

    edgeMap :: Map.Map (VertexId, VertexId) Edge
    edgeMap
      = Map.fromListWith (\(Edge a b fidsA) (Edge _ _ fidsB) -> Edge a b (fidsA ++ fidsB))
      . fmap (\edge@(Edge a b _) -> (edgeKey a b, edge))
      $ concat faceEdges

    edges :: EdgeArray
    edges = listArray (EdgeId 0, EdgeId $ (Map.size edgeMap) - 1) $ Map.elems edgeMap

    edgeIdMap :: Map.Map (VertexId, VertexId) EdgeId
    edgeIdMap = Map.fromList $ zipWith (\(Edge a b _) eid -> ((edgeKey a b), eid)) (elems edges) eids

    faceEdgeIds :: [[EdgeId]]
    faceEdgeIds = fmap (mapMaybe (\(Edge a b _) -> Map.lookup (edgeKey a b) edgeIdMap)) faceEdges

    faces :: FaceArray
    faces
      = listArray (FaceId 0, FaceId $ (length sFaces) - 1)
      $ zipWith (\(SimpleFace verts) edgeIds -> Face verts edgeIds) sFacesList faceEdgeIds

    vidsToFids :: Map.Map VertexId [FaceId]
    vidsToFids
      = Map.fromListWith (++)
      . concat
      $ zipWith (\(SimpleFace vertexIds) fid -> fmap (\vid -> (vid, [fid])) vertexIds) sFacesList fids

    vidsToEids :: Map.Map VertexId [EdgeId]
    vidsToEids
      = Map.fromListWith (++)
      . concat
      $ zipWith (\(Edge a b _) eid -> [(a, [eid]), (b, [eid])]) (elems edges) eids

    simpleToComplexVert :: SimpleVertex a -> VertexId -> Vertex a
    simpleToComplexVert (SimpleVertex point) vid
      = Vertex point
      (Map.findWithDefault [] vid vidsToEids)
      (Map.findWithDefault [] vid vidsToFids)

    vertices :: VertexArray a
    vertices
      = listArray (bounds sVertices)
      $ zipWith simpleToComplexVert (elems sVertices) (fmap VertexId [0..])

pShowSimpleMesh :: Show a => SimpleMesh a -> String
pShowSimpleMesh (SimpleMesh vertices faces)
  = "Vertices:\n" ++ (arrShow vertices sVertexPoint)
  ++ "Faces:\n" ++ (arrShow faces (fmap getVertexId . sFaceVertices))
  where
    arrShow a f = concatMap ((++ "\n") . show . (\(i, e) -> (i, f e))) . zip [0 :: Int ..] $ elems a

-- Testing types.
data Point a = Point a a a deriving (Show)

instance Functor Point where
  fmap f (Point x y z) = Point (f x) (f y) (f z)

zipPoint :: (a -> b -> c) -> Point a -> Point b -> Point c
zipPoint f (Point x y z) (Point x' y' z') = Point (f x x') (f y y') (f z z')

instance Num a => Num (Point a) where
  (+) = zipPoint (+)
  (-) = zipPoint (-)
  (*) = zipPoint (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger i = let i' = fromInteger i in Point i' i' i'

instance Fractional a => Fractional (Point a) where
  recip = fmap recip
  fromRational r = let r' = fromRational r in Point r' r' r'

testCube :: SimpleMesh (Point Double)
testCube = SimpleMesh vertices faces
  where
    vertices = listArray (VertexId 0, VertexId 7)
      $ fmap SimpleVertex
      [ Point (-1) (-1) (-1)
      , Point (-1) (-1) 1
      , Point (-1) 1 (-1)
      , Point (-1) 1 1
      , Point 1 (-1) (-1)
      , Point 1 (-1) 1
      , Point 1 1 (-1)
      , Point 1 1 1]
    faces = listArray (FaceId 0, FaceId 5)
      $ fmap (SimpleFace . (fmap VertexId))
      [ [0, 4, 5, 1]
      , [4, 6, 7, 5]
      , [6, 2, 3, 7]
      , [2, 0, 1, 3]
      , [1, 5, 7, 3]
      , [0, 2, 6, 4]]

testCubeWithHole :: SimpleMesh (Point Double)
testCubeWithHole
  = SimpleMesh (sMeshVertices testCube) (ixmap (FaceId 0, FaceId 4) id (sMeshFaces testCube))

testTriangle :: SimpleMesh (Point Double)
testTriangle = SimpleMesh vertices faces
  where
    vertices = listArray (VertexId 0, VertexId 2)
      $ fmap SimpleVertex
      [ Point 0 0 0
      , Point 0 0 1
      , Point 0 1 0]
    faces = listArray (FaceId 0, FaceId 0)
      $ fmap (SimpleFace . (fmap VertexId))
      [ [0, 1, 2]]

main :: IO ()
main = putStr . pShowSimpleMesh $ subdivide testCube
```

{{out}}

```txt
Vertices:
(0,Point 0.0 (-1.0) 0.0)
(1,Point 1.0 0.0 0.0)
(2,Point 0.0 1.0 0.0)
(3,Point (-1.0) 0.0 0.0)
(4,Point 0.0 0.0 1.0)
(5,Point 0.0 0.0 (-1.0))
(6,Point (-0.75) (-0.75) 0.0)
(7,Point (-0.75) 0.0 (-0.75))
(8,Point 0.0 (-0.75) (-0.75))
(9,Point (-0.75) 0.0 0.75)
(10,Point 0.0 (-0.75) 0.75)
(11,Point (-0.75) 0.75 0.0)
(12,Point 0.0 0.75 (-0.75))
(13,Point 0.0 0.75 0.75)
(14,Point 0.75 (-0.75) 0.0)
(15,Point 0.75 0.0 (-0.75))
(16,Point 0.75 0.0 0.75)
(17,Point 0.75 0.75 0.0)
(18,Point (-0.5555555555555556) (-0.5555555555555556) (-0.5555555555555556))
(19,Point (-0.5555555555555556) (-0.5555555555555556) 0.5555555555555556)
(20,Point (-0.5555555555555556) 0.5555555555555556 (-0.5555555555555556))
(21,Point (-0.5555555555555556) 0.5555555555555556 0.5555555555555556)
(22,Point 0.5555555555555556 (-0.5555555555555556) (-0.5555555555555556))
(23,Point 0.5555555555555556 (-0.5555555555555556) 0.5555555555555556)
(24,Point 0.5555555555555556 0.5555555555555556 (-0.5555555555555556))
(25,Point 0.5555555555555556 0.5555555555555556 0.5555555555555556)
Faces:
(0,[18,8,0,14])
(1,[22,14,0,10])
(2,[23,10,0,6])
(3,[19,6,0,8])
(4,[22,15,1,17])
(5,[24,17,1,16])
(6,[25,16,1,14])
(7,[23,14,1,15])
(8,[24,12,2,11])
(9,[20,11,2,13])
(10,[21,13,2,17])
(11,[25,17,2,12])
(12,[20,7,3,6])
(13,[18,6,3,9])
(14,[19,9,3,11])
(15,[21,11,3,7])
(16,[19,10,4,16])
(17,[23,16,4,13])
(18,[25,13,4,9])
(19,[21,9,4,10])
(20,[18,7,5,12])
(21,[20,12,5,15])
(22,[24,15,5,8])
(23,[22,8,5,7])
```



## J



```j
avg=: +/ % #

havePoints=: e."1/~ i.@#

catmullclark=:3 :0
  'mesh points'=. y
  face_point=. avg"2 mesh{points
  point_face=. |: mesh havePoints points
  avg_face_points=. point_face avg@#"1 2 face_point
  edges=. ~.,/ meshEdges=. mesh /:~@,"+1|."1 mesh
  edge_face=. *./"2 edges e."0 1/ mesh
  edge_center=. avg"2 edges{points
  edge_point=. (0.5*edge_center) + 0.25 * edge_face +/ .* face_point  
  point_edge=. |: edges havePoints points
  avg_mid_edges=.  point_edge avg@#"1 2 edge_center
  n=. +/"1 point_edge
  'm3 m2 m1'=. (2,1,:n-3)%"1 n
  new_coords=. (m1 * points) + (m2 * avg_face_points) + (m3 * avg_mid_edges)
  pts=. face_point,edge_point,new_coords
  c0=. (#edge_point)+ e0=. #face_point
  msh=. (,c0+mesh),.(,e0+edges i. meshEdges),.((#i.)~/$mesh),.,e0+_1|."1 edges i. meshEdges
  msh;pts
)
```


Example use:


```j
NB.cube
points=: _1+2*#:i.8
mesh=: 1 A."1 I.(,1-|.)8&$@#&0 1">4 2 1

   catmullclark mesh;points
┌──────────┬─────────────────────────────┐
│22  6 0  9│        1         0         0│
│23  7 0  6│        0         1         0│
│25  8 0  7│        0         0         1│
│24  9 0  8│        0         0        _1│
│20 10 1 12│        0        _1         0│
│21 11 1 10│       _1         0         0│
│25  8 1 11│     0.75     _0.75         0│
│24 12 1  8│     0.75         0      0.75│
│19 13 2 14│     0.75      0.75         0│
│21 11 2 13│     0.75         0     _0.75│
│25  7 2 11│    _0.75      0.75         0│
│23 14 2  7│        0      0.75      0.75│
│18 15 3 16│        0      0.75     _0.75│
│20 12 3 15│    _0.75         0      0.75│
│24  9 3 12│        0     _0.75      0.75│
│22 16 3  9│    _0.75         0     _0.75│
│18 17 4 16│        0     _0.75     _0.75│
│19 14 4 17│    _0.75     _0.75         0│
│23  6 4 14│_0.555556 _0.555556 _0.555556│
│22 16 4  6│_0.555556 _0.555556  0.555556│
│18 17 5 15│_0.555556  0.555556 _0.555556│
│19 13 5 17│_0.555556  0.555556  0.555556│
│21 10 5 13│ 0.555556 _0.555556 _0.555556│
│20 15 5 10│ 0.555556 _0.555556  0.555556│
│          │ 0.555556  0.555556 _0.555556│
│          │ 0.555556  0.555556  0.555556│
└──────────┴─────────────────────────────┘
```



## Julia


```julia
using Makie, Statistics

# Point3f0 is a 3-tuple of 32-bit floats for 3-dimensional space, and all Points are 3D.
Point = Point3f0

# a Face is defined by the points that are its vertices, in order.
Face = Vector{Point}

# an Edge is a line segment where the points are sorted
struct Edge
    p1::Point
    p2::Point
    Edge(a, b) = new(min(a, b), max(a, b))
end

edgemidpoint(edge) = (edge.p1  + edge.p2) / 2.0
facesforpoint(p, faces) = [f for f in faces if p in f]
facesforedge(e, faces) = [f for f in faces if (e.p1 in f) && (e.p2 in f)]
nexttohole(edge, faces) = length(facesforedge(edge, faces)) < 2

function newedgepoint(edge, faces)
    f = facesforedge(edge, faces)
    p1, p2, len = edge.p1, edge.p2, length(f)
    if len == 2
        return (p1 + p2 + mean(f[1]) + mean(f[2])) / 4.0
    elseif len == 1
        return (p1 + p2 + mean(f[1])) / 3.0
    end
    return (p1 + p2) / 2.0
end

function edgesforface(face)
    ret, indices = Vector{Edge}(), collect(1:length(face))
    for i in 1:length(face)-1
        push!(ret, Edge(face[indices[1]], face[indices[2]]))
        indices .= circshift(indices, 1)
    end
    ret
end

function edgesforpoint(p, faces)
    f = filter(x -> p in x, faces)
    return filter(e -> p == e.p1 || p == e.p2, mapreduce(edgesforface, vcat, f))
end

function adjacentpoints(point, face)
    a = indexin([point], face)
    if a[1] != nothing
        adjacent = (a[1] == 1) ? [face[end], face[2]] :
            a[1] == length(face) ? [face[end-1], face[1]] :
            [face[a[1] - 1], face[a[1] + 1]]
        return sort(adjacent)
    else
        throw("point $point not in face $face")
    end
end

adjacentedges(point, face) = [Edge(point, x) for x in adjacentpoints(point, face)]
facewrapped(face) = begin f = deepcopy(face); push!(f, f[1]); f end
drawface(face, colr) = lines(facewrapped(face); color=colr)
drawface!(face, colr) = lines!(facewrapped(face); color=colr)
drawfaces!(faces, colr) = for f in faces drawface!(f, colr) end
const colors = [:red, :green, :blue, :gold]

function drawfaces(faces, colr)
    scene = drawface(faces[1], colr)
    if length(faces) > 1
        for f in faces[2:end]
            drawface!(f, colr)
        end
    end
    scene
end

function catmullclarkstep(faces)
    d, E, dprime = Set(reduce(vcat, faces)), Dict{Vector, Point}(), Dict{Point, Point}()
    for face in faces, (i, p) in enumerate(face)
        edge = (p == face[end]) ? Edge(p, face[1]) : Edge(p, face[i + 1])
        E[[edge, face]] = newedgepoint(edge, faces)
    end
    for p in d
        F = mean([mean(face) for face in facesforpoint(p, faces)])
        pe = edgesforpoint(p, faces)
        R = mean(map(edgemidpoint, pe))
        n = length(pe)
        dprime[p] = (F + 2 * R + p * (n - 3)) / n
    end
    newfaces = Vector{Face}()
    for face in faces
        v = mean(face)
        for point in face
            fp1, fp2 = map(x -> E[[x, face]], adjacentedges(point, face))
            push!(newfaces, [fp1, dprime[point], fp2, v])
        end
    end
    return newfaces
end

"""
    catmullclark(faces, iters, scene)

Perform a multistep Catmull-Clark subdivision of a surface. See Wikipedia or page 53
of http://graphics.stanford.edu/courses/cs468-10-fall/LectureSlides/10_Subdivision.pdf
Plots the iterations, with colors for each iteration as set in the colors array.
Uses a Makie Scene of scene to plot the iters iterations.
"""
function catmullclark(faces, iters, scene)
    nextfaces = deepcopy(faces)
    for i in 1:iters
        nextfaces = catmullclarkstep(nextfaces)
        drawfaces!(nextfaces, colors[i])
        display(scene)
        sleep(1)
    end
end

const inputpoints = [
    [-1.0, -1.0, -1.0],
    [-1.0, -1.0, 1.0],
    [-1.0, 1.0, -1.0],
    [-1.0, 1.0, 1.0],
    [1.0, -1.0, -1.0],
    [1.0, -1.0, 1.0],
    [1.0, 1.0, -1.0],
    [1.0, 1.0, 1.0]
]

const inputfaces = [
    [0, 4, 5, 1],
    [4, 6, 7, 5],
    [6, 2, 3, 7],
    [2, 0, 1, 3],
    [1, 5, 7, 3],
    [0, 2, 6, 4]
]

const faces = [map(x -> Point3f0(inputpoints[x]), p .+ 1) for p in inputfaces]

scene = drawfaces(faces, :black)
display(scene)
sleep(1)

catmullclark(faces, 4, scene)

println("Press Enter to continue", readline())

```



## Mathematica

This implementation supports tris, quads, and higher polys, as well as surfaces with holes.
The function relies on three externally defined general functionality functions:


```Mathematica
subSetQ[large_,small_] := MemberQ[large,small]
subSetQ[large_,small_List] := And@@(MemberQ[large,#]&/@small)

containing[groupList_,item_]:= Flatten[Position[groupList,group_/;subSetQ[group,item]]]

ReplaceFace[face_]:=Transpose[Prepend[Transpose[{#[[1]],face,#[[2]]}&/@Transpose[Partition[face,2,1,1]//{#,RotateRight[#]}&]],face]]
```

subSetQ[small,large] is a boolean test for whether small is a subset of large. Note this is not a general purpose implimentation and only serves this purpose under the constrictions of the following program. 

containing[{obj1,obj2,...},item] Will return a list of indices of the objects containing item, where objects are faces or edges and item is edges or vertexes. 
faces containing a given vertex, faces containing a given edge,  edges containing a given point. 
It is used for each such purpose in the code called via infix notation, the specific usage is easily distinguised by variable names. For example faces~containing~edge would be a list of the indices for the faces containing the given edge.

ReplaceFace[face] replaces the face with a list of descriptions for the new faces. It will return a list containing mixed objects, vertexes, edges and faces where edges and faces referes to the new vertexes to be generated by the code. When the new vertexes have been appended to the updated old vertexes, these mixed objects will be recalcluated into correct indices into the new vertex list by the later defined function newIndex[]. 



```Mathematica
CatMullClark[{Points_,faces_}]:=Block[{avgFacePoints,avgEdgePoints,updatedPoints,newEdgePoints,newPoints,edges,newFaces,weights,pointUpdate,edgeUpdate,newIndex},
edges = DeleteDuplicates[Flatten[Partition[#,2,1,-1]&/@faces,1],Sort[#1]==Sort[#2]&];
avgFacePoints=Mean[Points[[#]]] &/@ faces;
avgEdgePoints=Mean[Points[[#]]] &/@ edges;

weights[vertex_]:= Count[faces,vertex,2]//{(#-3),1,2}/#&;
pointUpdate[vertex_]:= 
	If[Length[faces~containing~vertex]!=Length[edges~containing~vertex],
		Mean[avgEdgePoints[[Select[edges~containing~vertex,holeQ[edges[[#]],faces]&]]]],
		Total[weights[vertex]{ Points[[vertex]], Mean[avgFacePoints[[faces~containing~vertex]]], Mean[avgEdgePoints[[edges~containing~vertex]]]}]
	];

edgeUpdate[edge_]:= 
	If[Length[faces~containing~edge]==1,
		Mean[Points[[edge]]],
		Mean[Points[[Flatten[{edge, faces[[faces~containing~edge]]}]]]]
	];

updatedPoints = pointUpdate/@Range[1,Length[Points]];
newEdgePoints = edgeUpdate/@edges;
newPoints = Join[updatedPoints,avgFacePoints,newEdgePoints];

newIndex[edge_/;Length[edge]==2]  := Length[Points]+Length[faces]+Position[Sort/@edges,Sort@edge][[1,1]]
newIndex[face_] := Length[Points]+Position[faces,face][[1,1]]

newFaces  = Flatten[Map[newIndex[#,{Points,edges,faces}]&,ReplaceFace/@faces,{-2}],1];
{newPoints,newFaces}
]
```


The implimentation can be tested with polygons with and without holes by using the polydata 

```Mathematica
{points,faces}=PolyhedronData["Cube",{"VertexCoordinates","FaceIndices"}];

Function[iteration,
Graphics3D[(Polygon[iteration[[1]][[#]]]&/@iteration[[2]])]
]/@NestList[CatMullClark,{points,faces},3]//GraphicsRow
```

[[File:CAM noholes 1.png]]

For a surface with holes the resulting iterative subdivision will be:

```Mathematica
faces = Delete[faces, 6];
Function[iteration, Graphics3D[
    (Polygon[iteration[[1]][[#]]] & /@ iteration[[2]])
    ]] /@ NestList[CatMullClark, {points, faces}, 3] // GraphicsRow
```

[[File:CAM holes 1.png]]

This code was written in Mathematica 8.


## OCaml

{{incorrect|OCaml|wrong output data}}

The implementation below only supports quad faces, but it does handle surfaces with holes.

This code uses a module called '''Dynar''' (for dynamic array) because it needs a structure similar to arrays but with which we can push a new element at the end. (The source of this module is given in the [[Catmull–Clark subdivision surface/OCaml|sub-page]].)

In the [[Catmull–Clark subdivision surface/OCaml|sub-page]] there is also a program in OCaml+OpenGL which displays a cube subdivided 2 times with this algorithm.


```ocaml
open Dynar

let add3 (x1, y1, z1) (x2, y2, z2) (x3, y3, z3) =
  ( (x1 +. x2 +. x3),
    (y1 +. y2 +. y3),
    (z1 +. z2 +. z3) )

let mul m (x,y,z) = (m *. x, m *. y, m *. z)

let avg pts =
  let n, (x,y,z) =
    List.fold_left
      (fun (n, (xt,yt,zt)) (xi,yi,zi) ->
         succ n, (xt +. xi, yt +. yi, zt +. zi))
      (1, List.hd pts) (List.tl pts)
  in
  let n = float_of_int n in
  (x /. n, y /. n, z /. n)


let catmull ~points ~faces =
  let da_points = Dynar.of_array points in
  let new_faces = Dynar.of_array [| |] in
  let push_face face = Dynar.push new_faces face in
  let h1 = Hashtbl.create 43 in
  let h2 = Hashtbl.create 43 in
  let h3 = Hashtbl.create 43 in
  let h4 = Hashtbl.create 43 in
  let blg = Array.make (Array.length points) 0 in (* how many faces a point belongs to *)
  let f_incr p = blg.(p) <- succ blg.(p) in
  let eblg = Array.make (Array.length points) 0 in (* how many edges a point belongs to *)
  let e_incr p = eblg.(p) <- succ eblg.(p) in
  let edge a b = (min a b, max a b) in  (* suitable for hash-table keys *)
  let mid_edge p1 p2 =
    let x1, y1, z1 = points.(p1)
    and x2, y2, z2 = points.(p2) in
    ( (x1 +. x2) /. 2.0,
      (y1 +. y2) /. 2.0,
      (z1 +. z2) /. 2.0 )
  in
  let mid_face p1 p2 p3 p4 =
    let x1, y1, z1 = points.(p1)
    and x2, y2, z2 = points.(p2)
    and x3, y3, z3 = points.(p3)
    and x4, y4, z4 = points.(p4) in
    ( (x1 +. x2 +. x3 +. x4) /. 4.0,
      (y1 +. y2 +. y3 +. y4) /. 4.0,
      (z1 +. z2 +. z3 +. z4) /. 4.0 )
  in
  Array.iteri (fun i (a,b,c,d) ->
    f_incr a; f_incr b; f_incr c; f_incr d;

    let face_point = mid_face a b c d in
    let face_pi = pushi da_points face_point in
    Hashtbl.add h3 a face_point;
    Hashtbl.add h3 b face_point;
    Hashtbl.add h3 c face_point;
    Hashtbl.add h3 d face_point;

    let process_edge a b =
      let ab = edge a b in
      if not(Hashtbl.mem h1 ab)
      then begin
        let mid_ab = mid_edge a b in
        let index = pushi da_points mid_ab in
        Hashtbl.add h1 ab (index, mid_ab, [face_point]);
        Hashtbl.add h2 a mid_ab;
        Hashtbl.add h2 b mid_ab;
        Hashtbl.add h4 mid_ab 1;
        (index)
      end
      else begin
        let index, mid_ab, fpl = Hashtbl.find h1 ab in
        Hashtbl.replace h1 ab (index, mid_ab, face_point::fpl);
        Hashtbl.add h4 mid_ab (succ(Hashtbl.find h4 mid_ab));
        (index)
      end
    in
    let mid_ab = process_edge a b
    and mid_bc = process_edge b c
    and mid_cd = process_edge c d
    and mid_da = process_edge d a in

    push_face (a, mid_ab, face_pi, mid_da);
    push_face (b, mid_bc, face_pi, mid_ab);
    push_face (c, mid_cd, face_pi, mid_bc);
    push_face (d, mid_da, face_pi, mid_cd);
  ) faces;

  Hashtbl.iter (fun (a,b) (index, mid_ab, fpl) ->
    e_incr a; e_incr b;
    if List.length fpl = 2 then
      da_points.ar.(index) <- avg (mid_ab::fpl)
  ) h1;

  Array.iteri (fun i old_vertex ->
    let n = blg.(i)
    and e_n = eblg.(i) in
    (* if the vertex doesn't belongs to as many faces than edges
       this means that this is a hole *)
    if n = e_n then
    begin
      let avg_face_points =
        let face_point_list = Hashtbl.find_all h3 i in
        (avg face_point_list)
      in
      let avg_mid_edges = 
        let mid_edge_list = Hashtbl.find_all h2 i in
        (avg mid_edge_list)
      in
      let n = float_of_int n in
      let m1 = (n -. 3.0) /. n
      and m2 = 1.0 /. n
      and m3 = 2.0 /. n in
      da_points.ar.(i) <-
          add3 (mul m1 old_vertex)
               (mul m2 avg_face_points)
               (mul m3 avg_mid_edges)
    end
    else begin
      let mid_edge_list = Hashtbl.find_all h2 i in
      let mid_edge_list =
        (* only average mid-edges near the hole *)
        List.fold_left (fun acc mid_edge ->
          match Hashtbl.find h4 mid_edge with
          | 1 -> mid_edge::acc
          | _ -> acc
        ) [] mid_edge_list
      in
      da_points.ar.(i) <- avg (old_vertex :: mid_edge_list)
    end
  ) points;

  (Dynar.to_array da_points,
   Dynar.to_array new_faces)
;;
```


###  Another implementation 

Another implementation which should work with holes, but has only been tested on a cube
{{works with|OCaml|4.02+}}

```OCaml
type point = { x: float; y : float; z : float }
let zero = { x = 0.0; y = 0.0; z = 0.0 }
let add a b = { x = a.x+.b.x; y = a.y+.b.y; z = a.z+.b.z }
let mul a k = { x = a.x*.k; y = a.y*.k; z= a.z*.k }
let div p k = mul p (1.0/.k)

type face = Face of point list
type edge = Edge of point*point

let make_edge a b = Edge (min a b, max a b)
let make_face a b c d = Face [a;b;c;d]

let centroid plist = div (List.fold_left add zero plist) (float (List.length plist))
let mid_edge (Edge (p1,p2)) = div (add p1 p2) 2.0
let face_point (Face pl) = centroid pl
let point_in_face p (Face pl) = List.mem p pl
let point_in_edge p (Edge (p1,p2)) = p = p1 || p = p2
let edge_in_face (Edge (p1,p2)) f = point_in_face p1 f && point_in_face p2 f

let border_edge faces e =
   List.length (List.filter (edge_in_face e) faces) < 2

let edge_point faces e =
   if border_edge faces e then mid_edge e else
   let adjacent = List.filter (edge_in_face e) faces in
   let fps = List.map face_point adjacent in
   centroid [mid_edge e; centroid fps]

let mod_vertex faces edges p =
   let v_edges = List.filter (point_in_edge p) edges in
   let v_faces = List.filter (point_in_face p) faces in
   let n = List.length v_faces in
   let is_border = n <> (List.length v_edges) in
   if is_border then
      let border_mids = List.map mid_edge (List.filter (border_edge faces) v_edges) in
      (* description ambiguity: average (border+p) or average(average(border),p) ?? *)
      centroid (p :: border_mids)
   else
      let avg_face = centroid (List.map face_point v_faces) in
      let avg_mid = centroid (List.map mid_edge v_edges) in
      div (add (add (mul p (float(n-3))) avg_face) (mul avg_mid 2.0)) (float n)

let edges_of_face (Face pl) =
   let rec next acc = function
      | [] -> invalid_arg "empty face"
      | a :: [] -> List.rev (make_edge a (List.hd pl) :: acc)
      | a :: (b :: _ as xs) -> next (make_edge a b :: acc) xs in
   next [] pl

let catmull_clark faces =
   let module EdgeSet = Set.Make(struct type t = edge let compare = compare end) in
   let edges = EdgeSet.elements (EdgeSet.of_list (List.concat (List.map edges_of_face faces))) in
   let mod_face ((Face pl) as face) =
      let fp = face_point face in
      let ep = List.map (edge_point faces) (edges_of_face face) in
      let e_tl = List.hd (List.rev ep) in
      let vl = List.map (mod_vertex faces edges) pl in
      let add_facet (e', acc) v e = e, (make_face e' v e fp :: acc) in
      let _, new_faces = List.fold_left2 add_facet (e_tl, []) vl ep in
      List.rev new_faces in
   List.concat (List.map mod_face faces)

let show_faces fl =
   let pr_point p = Printf.printf " (%.4f, %.4f, %.4f)" p.x p.y p.z in
   let pr_face (Face pl) = print_string "Face:"; List.iter pr_point pl; print_string "\n" in
   (print_string "surface {\n"; List.iter pr_face fl; print_string "}\n")

let c p q r = let s i = if i = 0 then -1.0 else 1.0 in { x = s p; y = s q; z = s r } ;;
let cube = [
   Face [c 0 0 0; c 0 0 1; c 0 1 1; c 0 1 0]; Face [c 1 0 0; c 1 0 1; c 1 1 1; c 1 1 0];
   Face [c 0 0 0; c 1 0 0; c 1 0 1; c 0 0 1]; Face [c 0 1 0; c 1 1 0; c 1 1 1; c 0 1 1];
   Face [c 0 0 0; c 0 1 0; c 1 1 0; c 1 0 0]; Face [c 0 0 1; c 0 1 1; c 1 1 1; c 1 0 1] ] in
show_faces cube;
show_faces (catmull_clark cube)
```

with output:

```txt
surface {
Face: (-1.0000, -1.0000, -1.0000) (-1.0000, -1.0000, 1.0000) (-1.0000, 1.0000, 1.0000) (-1.0000, 1.0000, -1.0000)
Face: (1.0000, -1.0000, -1.0000) (1.0000, -1.0000, 1.0000) (1.0000, 1.0000, 1.0000) (1.0000, 1.0000, -1.0000)
Face: (-1.0000, -1.0000, -1.0000) (1.0000, -1.0000, -1.0000) (1.0000, -1.0000, 1.0000) (-1.0000, -1.0000, 1.0000)
Face: (-1.0000, 1.0000, -1.0000) (1.0000, 1.0000, -1.0000) (1.0000, 1.0000, 1.0000) (-1.0000, 1.0000, 1.0000)
Face: (-1.0000, -1.0000, -1.0000) (-1.0000, 1.0000, -1.0000) (1.0000, 1.0000, -1.0000) (1.0000, -1.0000, -1.0000)
Face: (-1.0000, -1.0000, 1.0000) (-1.0000, 1.0000, 1.0000) (1.0000, 1.0000, 1.0000) (1.0000, -1.0000, 1.0000)
}
surface {
Face: (-0.7500, 0.0000, -0.7500) (-0.5556, -0.5556, -0.5556) (-0.7500, -0.7500, 0.0000) (-1.0000, 0.0000, 0.0000)
Face: (-0.7500, -0.7500, 0.0000) (-0.5556, -0.5556, 0.5556) (-0.7500, 0.0000, 0.7500) (-1.0000, 0.0000, 0.0000)
Face: (-0.7500, 0.0000, 0.7500) (-0.5556, 0.5556, 0.5556) (-0.7500, 0.7500, 0.0000) (-1.0000, 0.0000, 0.0000)
Face: (-0.7500, 0.7500, 0.0000) (-0.5556, 0.5556, -0.5556) (-0.7500, 0.0000, -0.7500) (-1.0000, 0.0000, 0.0000)
Face: (0.7500, 0.0000, -0.7500) (0.5556, -0.5556, -0.5556) (0.7500, -0.7500, 0.0000) (1.0000, 0.0000, 0.0000)
Face: (0.7500, -0.7500, 0.0000) (0.5556, -0.5556, 0.5556) (0.7500, 0.0000, 0.7500) (1.0000, 0.0000, 0.0000)
Face: (0.7500, 0.0000, 0.7500) (0.5556, 0.5556, 0.5556) (0.7500, 0.7500, 0.0000) (1.0000, 0.0000, 0.0000)
Face: (0.7500, 0.7500, 0.0000) (0.5556, 0.5556, -0.5556) (0.7500, 0.0000, -0.7500) (1.0000, 0.0000, 0.0000)
Face: (-0.7500, -0.7500, 0.0000) (-0.5556, -0.5556, -0.5556) (0.0000, -0.7500, -0.7500) (0.0000, -1.0000, 0.0000)
Face: (0.0000, -0.7500, -0.7500) (0.5556, -0.5556, -0.5556) (0.7500, -0.7500, 0.0000) (0.0000, -1.0000, 0.0000)
Face: (0.7500, -0.7500, 0.0000) (0.5556, -0.5556, 0.5556) (0.0000, -0.7500, 0.7500) (0.0000, -1.0000, 0.0000)
Face: (0.0000, -0.7500, 0.7500) (-0.5556, -0.5556, 0.5556) (-0.7500, -0.7500, 0.0000) (0.0000, -1.0000, 0.0000)
Face: (-0.7500, 0.7500, 0.0000) (-0.5556, 0.5556, -0.5556) (0.0000, 0.7500, -0.7500) (0.0000, 1.0000, 0.0000)
Face: (0.0000, 0.7500, -0.7500) (0.5556, 0.5556, -0.5556) (0.7500, 0.7500, 0.0000) (0.0000, 1.0000, 0.0000)
Face: (0.7500, 0.7500, 0.0000) (0.5556, 0.5556, 0.5556) (0.0000, 0.7500, 0.7500) (0.0000, 1.0000, 0.0000)
Face: (0.0000, 0.7500, 0.7500) (-0.5556, 0.5556, 0.5556) (-0.7500, 0.7500, 0.0000) (0.0000, 1.0000, 0.0000)
Face: (0.0000, -0.7500, -0.7500) (-0.5556, -0.5556, -0.5556) (-0.7500, 0.0000, -0.7500) (0.0000, 0.0000, -1.0000)
Face: (-0.7500, 0.0000, -0.7500) (-0.5556, 0.5556, -0.5556) (0.0000, 0.7500, -0.7500) (0.0000, 0.0000, -1.0000)
Face: (0.0000, 0.7500, -0.7500) (0.5556, 0.5556, -0.5556) (0.7500, 0.0000, -0.7500) (0.0000, 0.0000, -1.0000)
Face: (0.7500, 0.0000, -0.7500) (0.5556, -0.5556, -0.5556) (0.0000, -0.7500, -0.7500) (0.0000, 0.0000, -1.0000)
Face: (0.0000, -0.7500, 0.7500) (-0.5556, -0.5556, 0.5556) (-0.7500, 0.0000, 0.7500) (0.0000, 0.0000, 1.0000)
Face: (-0.7500, 0.0000, 0.7500) (-0.5556, 0.5556, 0.5556) (0.0000, 0.7500, 0.7500) (0.0000, 0.0000, 1.0000)
Face: (0.0000, 0.7500, 0.7500) (0.5556, 0.5556, 0.5556) (0.7500, 0.0000, 0.7500) (0.0000, 0.0000, 1.0000)
Face: (0.7500, 0.0000, 0.7500) (0.5556, -0.5556, 0.5556) (0.0000, -0.7500, 0.7500) (0.0000, 0.0000, 1.0000)
}
```



## Python


```python

"""

Input and output are assumed to be in this form based on the talk
page for the task:

input_points = [
  [-1.0,  1.0,  1.0],
  [-1.0, -1.0,  1.0],
  [ 1.0, -1.0,  1.0],
  [ 1.0,  1.0,  1.0],
  [ 1.0, -1.0, -1.0],
  [ 1.0,  1.0, -1.0],
  [-1.0, -1.0, -1.0],
  [-1.0,  1.0, -1.0]
]

input_faces = [
  [0, 1, 2, 3],
  [3, 2, 4, 5],
  [5, 4, 6, 7],
  [7, 0, 3, 5],
  [7, 6, 1, 0],
  [6, 1, 2, 4],
]

So, the graph is a list of points and a list of faces.
Each face is a list of indexes into the points list.
   
"""

from mpl_toolkits.mplot3d import axes3d
import matplotlib.pyplot as plt
import numpy as np
import sys

def center_point(p1, p2):
    """ 
    returns a point in the center of the 
    segment ended by points p1 and p2
    """
    cp = []
    for i in range(3):
        cp.append((p1[i]+p2[i])/2)
        
    return cp
    
def sum_point(p1, p2):
    """ 
    adds points p1 and p2
    """
    sp = []
    for i in range(3):
        sp.append(p1[i]+p2[i])
        
    return sp

def div_point(p, d):
    """ 
    divide point p by d
    """
    sp = []
    for i in range(3):
        sp.append(p[i]/d)
        
    return sp
    
def mul_point(p, m):
    """ 
    multiply point p by m
    """
    sp = []
    for i in range(3):
        sp.append(p[i]*m)
        
    return sp

def get_face_points(input_points, input_faces):
    """
    From http://rosettacode.org/wiki/Catmull%E2%80%93Clark_subdivision_surface
    
    1. for each face, a face point is created which is the average of all the points of the face.
    """

    # 3 dimensional space
    
    NUM_DIMENSIONS = 3
    
    # face_points will have one point for each face
    
    face_points = []
    
    for curr_face in input_faces:
        face_point = [0.0, 0.0, 0.0]
        for curr_point_index in curr_face:
            curr_point = input_points[curr_point_index]
            # add curr_point to face_point
            # will divide later
            for i in range(NUM_DIMENSIONS):
                face_point[i] += curr_point[i]
        # divide by number of points for average
        num_points = len(curr_face)
        for i in range(NUM_DIMENSIONS):
            face_point[i] /= num_points
        face_points.append(face_point)
        
    return face_points
    
def get_edges_faces(input_points, input_faces):
    """
    
    Get list of edges and the one or two adjacent faces in a list.
    also get center point of edge
    
    Each edge would be [pointnum_1, pointnum_2, facenum_1, facenum_2, center]
    
    """
    
    # will have [pointnum_1, pointnum_2, facenum]
    
    edges = []
    
    # get edges from each face
    
    for facenum in range(len(input_faces)):
        face = input_faces[facenum]
        num_points = len(face)
        # loop over index into face
        for pointindex in range(num_points):
            # if not last point then edge is curr point and next point
            if pointindex < num_points - 1:
                pointnum_1 = face[pointindex]
                pointnum_2 = face[pointindex+1]
            else:
                # for last point edge is curr point and first point
                pointnum_1 = face[pointindex]
                pointnum_2 = face[0]
            # order points in edge by lowest point number
            if pointnum_1 > pointnum_2:
                temp = pointnum_1
                pointnum_1 = pointnum_2
                pointnum_2 = temp
            edges.append([pointnum_1, pointnum_2, facenum])
            
    # sort edges by pointnum_1, pointnum_2, facenum
    
    edges = sorted(edges)
    
    # merge edges with 2 adjacent faces
    # [pointnum_1, pointnum_2, facenum_1, facenum_2] or
    # [pointnum_1, pointnum_2, facenum_1, None]
    
    num_edges = len(edges)
    eindex = 0
    merged_edges = []
    
    while eindex < num_edges:
        e1 = edges[eindex]
        # check if not last edge
        if eindex < num_edges - 1:
            e2 = edges[eindex+1]
            if e1[0] == e2[0] and e1[1] == e2[1]:
                merged_edges.append([e1[0],e1[1],e1[2],e2[2]])
                eindex += 2
            else:
                merged_edges.append([e1[0],e1[1],e1[2],None])
                eindex += 1
        else:
            merged_edges.append([e1[0],e1[1],e1[2],None])
            eindex += 1
            
    # add edge centers
    
    edges_centers = []
    
    for me in merged_edges:
        p1 = input_points[me[0]]
        p2 = input_points[me[1]]
        cp = center_point(p1, p2)
        edges_centers.append(me+[cp])
            
    return edges_centers
       
def get_edge_points(input_points, edges_faces, face_points):
    """
    for each edge, an edge point is created which is the average 
    between the center of the edge and the center of the segment made
    with the face points of the two adjacent faces.
    """
    
    edge_points = []
    
    for edge in edges_faces:
        # get center of edge
        cp = edge[4]
        # get center of two facepoints
        fp1 = face_points[edge[2]]
        # if not two faces just use one facepoint
        # should not happen for solid like a cube
        if edge[3] == None:
            fp2 = fp1
        else:
            fp2 = face_points[edge[3]]
        cfp = center_point(fp1, fp2)
        # get average between center of edge and
        # center of facepoints
        edge_point = center_point(cp, cfp)
        edge_points.append(edge_point)      
        
    return edge_points
    
def get_avg_face_points(input_points, input_faces, face_points):
    """
    
    for each point calculate
    
    the average of the face points of the faces the point belongs to (avg_face_points)
    
    create a list of lists of two numbers [facepoint_sum, num_points] by going through the
    points in all the faces.
    
    then create the avg_face_points list of point by dividing point_sum (x, y, z) by num_points
    
    """
    
    # initialize list with [[0.0, 0.0, 0.0], 0]
    
    num_points = len(input_points)
    
    temp_points = []
    
    for pointnum in range(num_points):
        temp_points.append([[0.0, 0.0, 0.0], 0])
        
    # loop through faces updating temp_points
    
    for facenum in range(len(input_faces)):
        fp = face_points[facenum]
        for pointnum in input_faces[facenum]:
            tp = temp_points[pointnum][0]
            temp_points[pointnum][0] = sum_point(tp,fp)
            temp_points[pointnum][1] += 1
            
    # divide to create avg_face_points
    
    avg_face_points = []
    
    for tp in temp_points:
       afp = div_point(tp[0], tp[1])
       avg_face_points.append(afp)
       
    return avg_face_points
    
def get_avg_mid_edges(input_points, edges_faces):
    """
    
    the average of the centers of edges the point belongs to (avg_mid_edges)
    
    create list with entry for each point 
    each entry has two elements. one is a point that is the sum of the centers of the edges
    and the other is the number of edges. after going through all edges divide by
    number of edges.
    
    """
    
    # initialize list with [[0.0, 0.0, 0.0], 0]
    
    num_points = len(input_points)
    
    temp_points = []
    
    for pointnum in range(num_points):
        temp_points.append([[0.0, 0.0, 0.0], 0])
        
    # go through edges_faces using center updating each point
    
    for edge in edges_faces:
        cp = edge[4]
        for pointnum in [edge[0], edge[1]]:
            tp = temp_points[pointnum][0]
            temp_points[pointnum][0] = sum_point(tp,cp)
            temp_points[pointnum][1] += 1
    
    # divide out number of points to get average
    
    avg_mid_edges = []
        
    for tp in temp_points:
       ame = div_point(tp[0], tp[1])
       avg_mid_edges.append(ame)
       
    return avg_mid_edges

def get_points_faces(input_points, input_faces):
    # initialize list with 0
    
    num_points = len(input_points)
    
    points_faces = []
    
    for pointnum in range(num_points):
        points_faces.append(0)
        
    # loop through faces updating points_faces
    
    for facenum in range(len(input_faces)):
        for pointnum in input_faces[facenum]:
            points_faces[pointnum] += 1
            
    return points_faces

def get_new_points(input_points, points_faces, avg_face_points, avg_mid_edges):
    """
    
    m1 = (n - 3) / n
    m2 = 1 / n
    m3 = 2 / n
    new_coords = (m1 * old_coords)
               + (m2 * avg_face_points)
               + (m3 * avg_mid_edges)
                        
    """
    
    new_points =[]
    
    for pointnum in range(len(input_points)):
        n = points_faces[pointnum]
        m1 = (n - 3) / n
        m2 = 1 / n
        m3 = 2 / n
        old_coords = input_points[pointnum]
        p1 = mul_point(old_coords, m1)
        afp = avg_face_points[pointnum]
        p2 = mul_point(afp, m2)
        ame = avg_mid_edges[pointnum]
        p3 = mul_point(ame, m3)
        p4 = sum_point(p1, p2)
        new_coords = sum_point(p4, p3)
        
        new_points.append(new_coords)
        
    return new_points
    
def switch_nums(point_nums):
    """
    Returns tuple of point numbers
    sorted least to most
    """
    if point_nums[0] < point_nums[1]:
        return point_nums
    else:
        return (point_nums[1], point_nums[0])    

def cmc_subdiv(input_points, input_faces):
    # 1. for each face, a face point is created which is the average of all the points of the face.
    # each entry in the returned list is a point (x, y, z).
    
    face_points = get_face_points(input_points, input_faces)
    
    # get list of edges with 1 or 2 adjacent faces
    # [pointnum_1, pointnum_2, facenum_1, facenum_2, center] or
    # [pointnum_1, pointnum_2, facenum_1, None, center]
    
    edges_faces = get_edges_faces(input_points, input_faces)
    
    # get edge points, a list of points
    
    edge_points = get_edge_points(input_points, edges_faces, face_points)
                    
    # the average of the face points of the faces the point belongs to (avg_face_points)                
                    
    avg_face_points = get_avg_face_points(input_points, input_faces, face_points)
       
    # the average of the centers of edges the point belongs to (avg_mid_edges)
       
    avg_mid_edges = get_avg_mid_edges(input_points, edges_faces) 
       
    # how many faces a point belongs to
    
    points_faces = get_points_faces(input_points, input_faces)
    
    """
    
    m1 = (n - 3) / n
    m2 = 1 / n
    m3 = 2 / n
    new_coords = (m1 * old_coords)
               + (m2 * avg_face_points)
               + (m3 * avg_mid_edges)
                        
    """
        
    new_points = get_new_points(input_points, points_faces, avg_face_points, avg_mid_edges)
        
    """
    
    Then each face is replaced by new faces made with the new points,
    
    for a triangle face (a,b,c):
       (a, edge_point ab, face_point abc, edge_point ca)
       (b, edge_point bc, face_point abc, edge_point ab)
       (c, edge_point ca, face_point abc, edge_point bc)
       
    for a quad face (a,b,c,d):
       (a, edge_point ab, face_point abcd, edge_point da)
       (b, edge_point bc, face_point abcd, edge_point ab)
       (c, edge_point cd, face_point abcd, edge_point bc)
       (d, edge_point da, face_point abcd, edge_point cd)
       
    face_points is a list indexed by face number so that is
    easy to get.
    
    edge_points is a list indexed by the edge number
    which is an index into edges_faces.
    
    need to add face_points and edge points to 
    new_points and get index into each.
    
    then create two new structures
    
    face_point_nums - list indexes by facenum
    whose value is the index into new_points
    
    edge_point num - dictionary with key (pointnum_1, pointnum_2)
    and value is index into new_points
       
    """
    
    # add face points to new_points
    
    face_point_nums = []
    
    # point num after next append to new_points
    next_pointnum = len(new_points)
    
    for face_point in face_points:
        new_points.append(face_point)
        face_point_nums.append(next_pointnum)
        next_pointnum += 1
        
    # add edge points to new_points
    
    edge_point_nums = dict()
    
    for edgenum in range(len(edges_faces)):
        pointnum_1 = edges_faces[edgenum][0]
        pointnum_2 = edges_faces[edgenum][1]
        edge_point = edge_points[edgenum]
        new_points.append(edge_point)
        edge_point_nums[(pointnum_1, pointnum_2)] = next_pointnum
        next_pointnum += 1
    
    # new_points now has the points to output. Need new
    # faces
    
    """
    
    just doing this case for now:
    
    for a quad face (a,b,c,d):
       (a, edge_point ab, face_point abcd, edge_point da)
       (b, edge_point bc, face_point abcd, edge_point ab)
       (c, edge_point cd, face_point abcd, edge_point bc)
       (d, edge_point da, face_point abcd, edge_point cd)
       
    new_faces will be a list of lists where the elements are like this:
    
    [pointnum_1, pointnum_2, pointnum_3, pointnum_4]
    
    """
    
    new_faces =[]
    
    for oldfacenum in range(len(input_faces)):
        oldface = input_faces[oldfacenum]
        # 4 point face
        if len(oldface) == 4:
            a = oldface[0]
            b = oldface[1]
            c = oldface[2]
            d = oldface[3]
            face_point_abcd = face_point_nums[oldfacenum]
            edge_point_ab = edge_point_nums[switch_nums((a, b))]
            edge_point_da = edge_point_nums[switch_nums((d, a))]
            edge_point_bc = edge_point_nums[switch_nums((b, c))]
            edge_point_cd = edge_point_nums[switch_nums((c, d))]
            new_faces.append((a, edge_point_ab, face_point_abcd, edge_point_da))
            new_faces.append((b, edge_point_bc, face_point_abcd, edge_point_ab))
            new_faces.append((c, edge_point_cd, face_point_abcd, edge_point_bc))
            new_faces.append((d, edge_point_da, face_point_abcd, edge_point_cd))    
    
    return new_points, new_faces
    

def graph_output(output_points, output_faces):
    
    fig = plt.figure()
    ax = fig.add_subplot(111, projection='3d')
    
    """
    
    Plot each face
    
    """
    
    for facenum in range(len(output_faces)):
        curr_face = output_faces[facenum]
        xcurr = []
        ycurr = []
        zcurr = []
        for pointnum in range(len(curr_face)):
            xcurr.append(output_points[curr_face[pointnum]][0])
            ycurr.append(output_points[curr_face[pointnum]][1])
            zcurr.append(output_points[curr_face[pointnum]][2])
        xcurr.append(output_points[curr_face[0]][0])
        ycurr.append(output_points[curr_face[0]][1])
        zcurr.append(output_points[curr_face[0]][2])
        
        ax.plot(xcurr,ycurr,zcurr,color='b')
    
    plt.show()


# cube

input_points = [
  [-1.0,  1.0,  1.0],
  [-1.0, -1.0,  1.0],
  [ 1.0, -1.0,  1.0],
  [ 1.0,  1.0,  1.0],
  [ 1.0, -1.0, -1.0],
  [ 1.0,  1.0, -1.0],
  [-1.0, -1.0, -1.0],
  [-1.0,  1.0, -1.0]
]

input_faces = [
  [0, 1, 2, 3],
  [3, 2, 4, 5],
  [5, 4, 6, 7],
  [7, 0, 3, 5],
  [7, 6, 1, 0],
  [6, 1, 2, 4],
]

if len(sys.argv) != 2:
    print("Should have one argument integer number of iterations")
    sys.exit()
else:
    iterations = int(sys.argv[1])
    
    output_points, output_faces = input_points, input_faces
    
    for i in range(iterations):
        output_points, output_faces = cmc_subdiv(output_points, output_faces)
        
graph_output(output_points, output_faces)

```



## Tcl

This code handles both holes and arbitrary polygons in the input data.

```tcl
package require Tcl 8.5

# Use math functions and operators as commands (Lisp-like).
namespace path {tcl::mathfunc tcl::mathop}

# Add 3 points.
proc add3 {A B C} {
    lassign $A Ax Ay Az
    lassign $B Bx By Bz
    lassign $C Cx Cy Cz
    list [+ $Ax $Bx $Cx] [+ $Ay $By $Cy] [+ $Az $Bz $Cz]
}

# Multiply a point by a constant.
proc mulC {m A} {
    lassign $A x y z
    list [* $m $x] [* $m $y] [* $m $z]
}

# Take the centroid of a set of points.
# Note that each of the arguments is a *list* of coordinate triples
# This makes things easier later.
proc centroid args {
    set x [set y [set z 0.0]]
    foreach plist $args {
	incr n [llength $plist]
	foreach p $plist {
	    lassign $p px py pz
	    set x [+ $x $px]
	    set y [+ $y $py]
	    set z [+ $z $pz]
	}
    }
    set n [double $n]
    list [/ $x $n] [/ $y $n] [/ $z $n]
}

# Select from the list the value from each of the indices in the *lists*
# in the trailing arguments.
proc selectFrom {list args} {
    foreach is $args {foreach i $is {lappend r [lindex $list $i]}}
    return $r
}

# Rotate a list.
proc lrot {list {n 1}} {
    set n [% $n [llength $list]]
    list {*}[lrange $list $n end] {*}[lrange $list 0 [incr n -1]]
}

# Generate an edge by putting the smaller coordinate index first.
proc edge {a b} {
    list [min $a $b] [max $a $b]
}

# Perform one step of Catmull-Clark subdivision of a surface.
proc CatmullClark {points faces} {
    # Generate the new face-points and list of edges, plus some lookup tables.
    set edges {}
    foreach f $faces {
	set ps [selectFrom $points $f]
	set fp [centroid $ps]
	lappend facepoints $fp
	foreach p $ps {
	    lappend fp4p($p) $fp
	}
	foreach p1 $f p2 [lrot $f] {
	    set e [edge $p1 $p2]
	    if {$e ni $edges} {
		lappend edges $e
	    }
	    lappend fp4e($e) $fp
	}
    }

    # Generate the new edge-points and mid-points of edges, and a few more
    # lookup tables.
    set i [+ [llength $points] [llength $faces]]
    foreach e $edges {
	set ep [selectFrom $points $e]
	if {[llength $fp4e($e)] > 1} {
	    set mid [centroid $ep $fp4e($e)]
	} else {
	    set mid [centroid $ep]
	    foreach p $ep {
		lappend ep_heavy($p) $mid
	    }
	}
	lappend edgepoints $mid
	set en4e($e) $i
	foreach p $ep {
	    lappend ep4p($p) $mid
	}
	incr i
    }

    # Generate the new vertex points with our lookup tables.
    foreach p $points {
	if {[llength $fp4p($p)] >= 4} {
	    set n [llength $fp4p($p)]
	    lappend newPoints [add3 [mulC [/ [- $n 3.0] $n] $p] \
		    [mulC [/ 1.0 $n] [centroid $fp4p($p)]] \
		    [mulC [/ 2.0 $n] [centroid $ep4p($p)]]]
	} else {
	    # Update a point on the edge of a hole. This formula is not
	    # described on the WP page, but produces a nice result.
	    lappend newPoints [centroid $ep_heavy($p) [list $p $p]]
	}
    }

    # Now compute the new set of quadrilateral faces.
    set i [llength $points]
    foreach f $faces {
	foreach a $f b [lrot $f] c [lrot $f -1] {
	    lappend newFaces [list \
		    $a $en4e([edge $a $b]) $i $en4e([edge $c $a])]
	}
	incr i
    }

    list [concat $newPoints $facepoints $edgepoints] $newFaces
}
```


The [[/Tcl Test Code|test code]] for this solution is available as well. The example there produces the following partial toroid output image:

<center>[[File:Tcl-Catmull.png]]</center>

[[Category:Geometry]]
