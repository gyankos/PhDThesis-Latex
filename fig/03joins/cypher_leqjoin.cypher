MATCH (src1)-[:r]->(dst1),
     (src2)-[:r]->(dst2)
WHERE src1.Year1<=src2.Year2 AND dst1.Year1<=dst2.Year2 AND src1.graph='L' AND src2.graph='R' AND dst1.graph='L' AND dst2.graph='R'
CREATE p=(:U {Organization1:src1.Organization1, Organization2:src2.Organization2 , Year1:src1.Year1, Year2:src2.Year2 ,  MyGraphLabel:"U-"})-[:r]->(:U {Organization1:dst1.Organization1, Organization2:dst2.Organization2 , Year1:dst1.Year1, Year2:dst2.Year2, MyGraphLabel:"U-"}) return p
UNION ALL
MATCH (src1)-[:r]->(u), (src2)-[:r]->(v)
WHERE  src1.Year1<=src2.Year2 AND src1.graph='L' AND src2.graph='R' AND (( u.Year1>v.Year2))
CREATE p=(:U {Organization1:src1.Organization1, Organization2:src2.Organization2 , Year1:src1.Year1, Year2:src2.Year2 , MyGraphLabel:"U-"}) return p
UNION ALL
MATCH (src1)-[:r]->(u), (src2)
WHERE src1.Year1<=src2.Year2 AND src1.graph='L' AND src2.graph='R' AND (NOT ((src2)-[:r]->()))
CREATE p=(:U {Organization1:src1.Organization1, Organization2:src2.Organization2 , Year1:src1.Year1, Year2:src2.Year2 , MyGraphLabel:"U-"}) return p
UNION ALL
MATCH (src1), (src2)-[:r]->(v)
WHERE src1.Year1<=src2.Year2 AND dst1.Year1<=dst2.Year2 AND src1.graph='L' AND src2.graph='R' AND (NOT ((src1)-[:r]->()))
CREATE p=(:U {Organization1:src1.Organization1, Organization2:src2.Organization2 , Year1:src1.Year1, Year2:src2.Year2 , MyGraphLabel:"U-"}) return p
UNION ALL
MATCH (src1), (src2)
WHERE src1.Year1<=src2.Year2 AND src1.graph='L' AND src2.graph='R' AND (NOT ((src2)-[:r]->())) AND (NOT ((src1)-[:r]->()))
CREATE p=(:U {Organization1:src1.Organization1, Organization2:src2.Organization2 , Year1:src1.Year1, Year2:src2.Year2 , MyGraphLabel:"U-"}) return p
