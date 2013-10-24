module Diamond where

import qualified Data.Set as S

type Vertex = (Float,Float,Float)
diamondSquare :: Int -> (Vertex,Vertex,Vertex,Vertex) -> S.Set Vertex
diamondSquare c verts@(nw,ne,se,sw) = 
	S.union (diamond c verts) $ S.fromList [nw,ne,se,sw]
	where
	diamond :: Int -> (Vertex,Vertex,Vertex,Vertex) -> S.Set Vertex
	diamond 0 _ = S.empty
	diamond c (nw@(x1,y1,z1),ne@(x2,y2,z2),se@(x3,y3,z3),sw@(x4,y4,z4)) = 
		S.unions [nwd,ned,sed,sew,S.fromList [n,e,s,w,center]]
		where
		center = ((x1+x2+x3+x4)/4,(y1+y2+y3+y4)/4,(z1+z2+z3+z4)/4)
		n = ((x1+x2)/2,(y1+y2)/2,(z1+z2)/2)
		s = ((x3+x4)/2,(y3+y4)/2,(z3+z4)/2)
		e = ((x2+x3)/2,(y2+y3)/2,(z2+z3)/2)
		w = ((x1+x4)/2,(y1+y4)/2,(z1+z4)/2)
		nwd = diamond (c-1) (nw,n,center,w)
		ned = diamond (c-1) (n,ne,e,center)
		sed = diamond (c-1) (center,e,se,s)
		sew = diamond (c-1) (w,center,s,sw)

main = print . S.size $ diamondSquare 10 ((0,0,0),(1,0,1),(1,1,0),(0,1,1))
