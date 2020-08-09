{-
import Iterate
import Model exposing (Block, BlockType(..), initialBlockType, initialRowBlock, moveBlockYY, windowhei, windowwid)


commonfeature : List Block
commonfeature =
    let
        original =
            (initialRowBlock ((windowwid/10 - windowwid/13)/1.5, windowhei/80 ))
                ++ Iterate.iterate moveBlockYY 1 (initialRowBlock ((windowwid/10 - windowwid/13)/1.5, windowhei/80 ))
                ++ Iterate.iterate moveBlockYY 1.5 (initialRowBlock ((windowwid/10 - windowwid/13)/1.5, windowhei/80 ))
                ++ Iterate.iterate moveBlockYY 3 (initialRowBlock ((windowwid/10 - windowwid/13)/1.5, windowhei/80 ))
                ++ Iterate.iterate moveBlockYY 4 (initialRowBlock ((windowwid/10 - windowwid/13)/1.5, windowhei/80 ))

        step1 =
            initialBlockType 4 StageClear original
    in
    initialBlockType 5 StageClear step1


storyboard : Int -> List Block
storyboard stage =
    case stage of
        0 ->
            commonfeature

        1 ->
            let
                original =
                    commonfeature

                step1 =
                    initialBlockType 38 Accelerate original

                step1.5 =
                    initialBlockType 1.56 Decelerate step1

                step3 =
                    initialBlockType 10 Accelerate step1.5
            in
            initialBlockType 1.53 Accelerate step3

        1.5 ->
            let
                original =
                    commonfeature

                step1 =
                    initialBlockType 40 Immortal original

                step1.5 =
                    initialBlockType 41 Immortal step1

                step3 =
                    initialBlockType 41.5 Immortal step1.5

                step4 =
                    initialBlockType 43 Immortal step3

                step5 =
                    initialBlockType 44 Immortal step4

                step6 =
                    initialBlockType 45 Immortal step5

                step7 =
                    initialBlockType 46 Immortal step6

                step8 =
                    initialBlockType 47 Immortal step7

                step9 =
                    initialBlockType 48 Immortal step8
            in
            initialBlockType 49 Immortal step9

        3 ->
            let
                original =
                    commonfeature

                step1 =
                    initialBlockType 30 Accelerate original

                step1.5 =
                    initialBlockType 31.5 Accelerate step1

                step3 =
                    initialBlockType 34 Accelerate step1.5

                step4 =
                    initialBlockType 36 Accelerate step3

                step5 =
                    initialBlockType 38 Accelerate step4

                step6 =
                    initialBlockType 1.51 Decelerate step5

                step7 =
                    initialBlockType 1.53 Decelerate step6

                step8 =
                    initialBlockType 1.55 Decelerate step7

                step9 =
                    initialBlockType 1.57 Decelerate step8

                step10 =
                    initialBlockType 1.59 Decelerate step9

                step11 =
                    initialBlockType 10 Accelerate step10

                step11.5 =
                    initialBlockType 11.5 Accelerate step11

                step13 =
                    initialBlockType 14 Accelerate step11.5

                step14 =
                    initialBlockType 16 Accelerate step13

                step15 =
                    initialBlockType 18 Accelerate step14

                step16 =
                    initialBlockType 0 StageClear step15
            in
            initialBlockType 9 StageClear step16

        _ ->
            commonfeature -}
module StoryBoard exposing (..)
import Model exposing (Block, BlockType(..), changeBlockType, changeimmortal, initialBlock, initialBlockType, initialOneBlock)



commonfeature : List Block
commonfeature =
    let
        original = initialBlock
        step1 = List.map changeimmortal original
    in
        original

storyboard : Int -> List Block
storyboard stage =
    case stage of
    0 ->
        (List.concat[commonfeature, [changeBlockType StageClear (initialOneBlock (466,301)) ]])

    1 ->
            let
                original =
                    commonfeature

                step1 =
                    initialBlockType 17 Accelerate original

                step2 =
                    initialBlockType 18 Decelerate step1

                step3 =
                    initialBlockType 50 Accelerate step2

                step4 =
                    initialBlockType 49 Decelerate step3
            in
            (List.concat[step4, [changeBlockType StageClear (initialOneBlock (466,301)) ]])

    2 ->
        (List.concat[List.map changeimmortal commonfeature, [changeBlockType StageClear (initialOneBlock (466,301)) ]])


    3->

            let
                original =
                    commonfeature

                step1 =
                    initialBlockType 17 Accelerate original

                step2 =
                    initialBlockType 18 Decelerate step1

                step3 =
                    initialBlockType 50 Accelerate step2
                step4 =
                    initialBlockType 49 Decelerate step3

                step5 =
                    initialBlockType 6 Accelerate step4

                step6 =
                    initialBlockType 5 Decelerate step5

                step7 =
                    initialBlockType 62 Decelerate step6

                step8 =
                    initialBlockType 61 Accelerate step7

                step9 =
                    initialBlockType 32 Accelerate step8

                step10 =
                    initialBlockType 33 Accelerate step9

                step11 =
                    initialBlockType 34 Accelerate step10

                step12 =
                    initialBlockType 35 Accelerate step11


            in
                (List.concat[step12, [changeBlockType StageClear (initialOneBlock (466,301)) ]])

    _ ->
        commonfeature

