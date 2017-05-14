import Data.Map

--average list = realToFrac (sum list) /  genericLength list
average xs = (sum xs) / (fromIntegral (length xs))
 
ranking xs =
        let avg = average xs
            len = fromIntegral (length xs)
            res1 = avg + len
        in res1

calc_each_rank xs = 
        let item_id = fst xs
            score = ranking (Prelude.map snd (snd xs))
        in (item_id,score)

pack_rank_result [] = [] 
pack_rank_result (x:xs) = [calc_each_rank x] ++ (pack_rank_result xs) 

calc_rank xs = Prelude.map calc_each_rank (toList xs)

--main = do
--    let
--        score_map = fromList [(1001,[(1,5),(2,4),(3,3)]),(1002,[(1,2),(2,3),(3,3)])]
--        score_list = toList score_map
--        l = head score_list
----        res = pack_rank_result score_list
--        res = Prelude.map calc_each_rank score_list
--        res2 = calc_rank score_map
--{-
--        p = Prelude.map snd score_list
--        b = ranking p
---}
--        -- p = ranking days
--    print res2
