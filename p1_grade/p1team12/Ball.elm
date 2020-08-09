module Ball exposing (..)

import Debug exposing (toString)
import Svg exposing (Svg, image)
import Svg.Attributes exposing (height, width, x, xlinkHref, y)
import Debug exposing (toString)
import String
import List exposing (..)
import Random

import Block exposing (..)
import Model exposing (..)
import Skill exposing (..)

--organize <number>Ball functions
getBallFunction: Int -> (Model -> String -> (Ball, Model))
getBallFunction number=
    case number of
        1 -> oneBall
        2 -> twoBall
        3 -> threeBall
        4 -> fourBall
        5 -> fiveBall
        6 -> sixBall
        7 -> sevenBall
        _ -> oneBall

-- Whether ball or file crash the paddle pr not
judgePaddleCrash : Model -> Ball -> String
judgePaddleCrash model ball=
    let
       gap = (distance (ball.xPos,ball.yPos) ( (minimalPointX ball.xPos (model.paddle.xPos - 1) (model.paddle.width + 2)) ,(minimalPointY ball.yPos (model.paddle.yPos) 1.2)))
    in
        if gap <= 1.2 then
            if ball.yPos < model.paddle.yPos then
                "S"
            else if ball.xPos > model.paddle.xPos + 5 then
                "W"
            else if ball.xPos < model.paddle.xPos then
                "E"
            else
                "No"
        else
            "No"

--move a given ball
moveOneBall : Model-> Ball -> (Model, Ball)
moveOneBall model ball=
    let
        ball0 = ball
        crashN = {ball | dy = abs(ball0.dy), yPos = ball0.yPos + 0.7}
        v = sqrt( (ball.dx)^2 + (ball.dy)^2 )
        calculateDx = sqrt(3)/(model.paddle.width + 2)*(ball0.xPos - model.paddle.xPos + 1) * v - sqrt(3)/2 * v
        crashPaddle = {ball | dy = -1 * sqrt(v^2 - calculateDx^2), dx = calculateDx ,yPos = ball0.yPos - 0.7 }
        crashS = {ball | dy = -1 * abs(ball0.dy), yPos = ball0.yPos - 0.7}
        crashW = {ball | dx = abs(ball0.dx), xPos = ball0.xPos + 0.7}
        crashE = {ball | dx = -1 * abs(ball0.dx), xPos = ball0.xPos - 0.7}
        keepFlying = {ball | xPos = ball0.xPos + ball0.dx, yPos = ball0.yPos + ball0.dy}
        x = ball.xPos
        y = ball.yPos
        seedTuple = Random.step (Random.int 0 100) model.seed
        lastProb = model.prob
        seedModel = { model | prob = Tuple.first seedTuple, seed = Tuple.second seedTuple, lastProb = lastProb}
        direction = Tuple.first (blockCrash seedModel ball)
        updatedModel = Tuple.second (blockCrash seedModel ball)
        model0 = {model | lastProb = lastProb}
    in
        if y > 55 then
            (model0, { ball | ballState = PaddleToBlock, yPos = 65,dx = 0, dy = 0 })
        else if List.isEmpty ( List.filter (\block -> block.exist) model.blocks ) then
            if model0.stage < 6 then
                ({ model0 | state = WaitNextStage }, { ball | ballState = PaddleToBlock })
            else
                ({ model0 | state = Win }, { ball | ballState = PaddleToBlock })
        else if judgePaddleCrash model ball == "S" then
            (model0, { crashPaddle | ballState = PaddleToBlock })
        else if judgePaddleCrash model ball == "W" then
            --(model0, { crashW | ballState = PaddleToBlock })
            (model0, { ball | ballState = PaddleToBlock, yPos = 65,dx = 0, dy = 0 })
        else if judgePaddleCrash model ball == "E" then
            --(model0, { crashE | ballState = PaddleToBlock })
            (model0, { ball | ballState = PaddleToBlock, yPos = 65,dx = 0, dy = 0 })

        else if direction == "S" then
            (updatedModel, { crashS | ballState = BlockToPaddle })
        else if x <= 1 && x>= -6 then --|| (direction == "E") then
            (model0, crashW)

        else if direction == "E" || judgePaddleCrash model ball == "E" then
            (updatedModel, { crashE | ballState = BlockToPaddle })
        else if x >= 74 then --|| (direction == "W") then
            (model0, crashE)
        else if direction == "W" || judgePaddleCrash model ball == "W" then
            (updatedModel, { crashW | ballState = BlockToPaddle })
        else if y <= 1.4 then --(direction == "N") || (y <= 0.4) then
            (model0, crashN)
        else if direction == "N" then
            (updatedModel, { crashN | ballState = BlockToPaddle })
        else
            (model0, keepFlying)


-- organize almost all the other functions in Ball.elm to move all the balls
moveBalls : Model -> Model
moveBalls model =
    let

        modelDiff = judgeDiff model
        modelRS = resetSpeed modelDiff
        modelJL = judgeLose modelRS
        modelSkill = useSkill modelJL
        model0 = judgeCreateBall2 modelSkill

        ball1 = Tuple.second ( moveOneBall model0 (Tuple.first (oneBall model0 "b")) )
        ball2 = Tuple.second ( moveOneBall upDatedModel1 (Tuple.first(twoBall upDatedModel1 "b")) )
        ball3 = Tuple.second ( moveOneBall upDatedModel2 (Tuple.first (threeBall upDatedModel2 "b")) )
        ball4 = Tuple.second ( moveOneBall upDatedModel3 (Tuple.first(fourBall upDatedModel3 "b")) )
        ball5 = Tuple.second ( moveOneBall upDatedModel4 (Tuple.first (fiveBall upDatedModel4 "b")) )
        ball6 = Tuple.second ( moveOneBall upDatedModel5 (Tuple.first(sixBall upDatedModel5 "b")) )
        ball7 = Tuple.second ( moveOneBall upDatedModel6 (Tuple.first (sevenBall upDatedModel6 "b")) )

        upDatedModel1 = chooseModel model0 ball1 1
        upDatedModel2 = chooseModel upDatedModel1 ball2 2
        upDatedModel3 = chooseModel upDatedModel2 ball3 3
        upDatedModel4 = chooseModel upDatedModel3 ball4 4
        upDatedModel5 = chooseModel upDatedModel4 ball5 5
        upDatedModel6 = chooseModel upDatedModel5 ball6 6
        upDatedModel7 = chooseModel upDatedModel6 ball7 7

    in
        {upDatedModel7 | balls = [(chooseBall (Tuple.first (oneBall model "b")) ball1 ball1.exist), (chooseBall (Tuple.first (twoBall model "b")) ball2 ball2.exist),(chooseBall (Tuple.first (threeBall model "b")) ball3 ball3.exist),(chooseBall (Tuple.first (fourBall model "b")) ball4 ball4.exist),(chooseBall (Tuple.first (fiveBall model "b")) ball5 ball5.exist),(chooseBall (Tuple.first (sixBall model "b")) ball6 ball6.exist),(chooseBall (Tuple.first (sevenBall model "b")) ball7 ball7.exist)] }

--reset speed with respect to difficulty
resetSpeed : Model -> Model
resetSpeed model =
    let
        ball1 = Tuple.first (oneBall model "b")
        ball2 = Tuple.first (twoBall model "b")
        ball3 = Tuple.first (threeBall model "b")
        ball4 = Tuple.first (fourBall model "b")
        ball5 = Tuple.first (fiveBall model "b")
        ball6 = Tuple.first (sixBall model "b")
        ball7 = Tuple.first (sevenBall model "b")

        newBall1 = {ball1 | dx = 0, dy = -1 * model.diffClass.ballSpeed * sqrt(2)}
        newBall2 = {ball2 | dx = model.diffClass.ballSpeed, dy = -1 * model.diffClass.ballSpeed}
        newBall3 = {ball3 | dx = model.diffClass.ballSpeed, dy = -1 * model.diffClass.ballSpeed}
        newBall4 = {ball4 | dx = model.diffClass.ballSpeed, dy = -1 * model.diffClass.ballSpeed}
        newBall5 = {ball5 | dx = model.diffClass.ballSpeed, dy = -1 * model.diffClass.ballSpeed}
        newBall6 = {ball6 | dx = model.diffClass.ballSpeed, dy = -1 * model.diffClass.ballSpeed}
        newBall7 = {ball7 | dx = model.diffClass.ballSpeed, dy = -1 * model.diffClass.ballSpeed}

    in
        if ball1.dx == 0.25 then
            {model | balls = [newBall1, newBall2, newBall3, newBall4, newBall5, newBall6, newBall7]}
        else
            model

--Whether we can create the second ball or not
judgeCreateBall2 : Model -> Model
judgeCreateBall2 model =
    let
        ballList = List.filter (\ball -> ball.exist == True && ball.yPos < 59 ) model.balls
        ball1 = Tuple.first (oneBall {model | balls = ballList} "b")
        ball2 = Tuple.first (twoBall model "b")
        otherBalls = List.filter (\ball -> ball /= ball1 && ball /= ball2) model.balls
    in
        if modBy (probWrtDiff model) model.prob == 0 && ball2.exist == False && (List.length ballList) == 1 && ball1.dx /= 0 then
            {model | balls = List.append [ball1, {ball2 | exist = True, xPos = ball1.xPos + 0.1, yPos = ball1.yPos + 0.1, dx = model.diffClass.ballSpeed * ball1.dx / abs(ball1.dx), dy = model.diffClass.ballSpeed * ball1.dy / abs(ball1.dy)  }]  otherBalls }
        else if modBy (probWrtDiff model) model.prob == 0 && ball2.exist == True && ball2.yPos >=59 && model.prob /= model.lastProb  && (List.length ballList) == 1  && ball1.dx /= 0 then
            {model | balls = List.append [ball1, {ball2 | exist = True, xPos = ball1.xPos + 0.1, yPos = ball1.yPos + 0.1, dx = model.diffClass.ballSpeed * ball1.dx / abs(ball1.dx), dy = model.diffClass.ballSpeed * ball1.dy / abs(ball1.dy)  }]  otherBalls }
        else if modBy (probWrtDiff model) model.prob == 0 && ball2.exist == True && ball1.yPos >=59 && model.prob /= model.lastProb  && (List.length ballList) == 1  && ball2.dx /= 0 then
            {model | balls = List.append [{ball1 | exist = True, xPos = ball2.xPos + 0.1, yPos = ball2.yPos + 0.1, dx = model.diffClass.ballSpeed * ball2.dx / abs(ball2.dx), dy = model.diffClass.ballSpeed * ball2.dy / abs(ball2.dy)  },ball2]  otherBalls }
        else
            {model | balls = List.append [ball1, ball2] otherBalls }

--the probability pf create the second ball
probWrtDiff : Model -> Int
probWrtDiff model =
    case model.difficulty of
        Easy ->
            10
        Medium ->
            7
        Difficult ->
            5

--Lose the game or just Lose one life
judgeLose : Model -> Model
judgeLose model =
    let
        ball1 = Tuple.first (oneBall model "b")
        ball2 = Tuple.first (twoBall model "b")
        ball3 = Tuple.first (threeBall model "b")
        ball4 = Tuple.first (fourBall model "b")
        ball5 = Tuple.first (fiveBall model "b")
        ball6 = Tuple.first (sixBall model "b")
        ball7 = Tuple.first (sevenBall model "b")

        oldPlayer = model.player
        newPlayer = { oldPlayer | lives = oldPlayer.lives - 1 }
    in
        if (ball1.yPos > 56 && ball2.exist == False && ball3.exist == False && ball4.exist == False && ball5.exist == False && ball6.exist == False && ball7.exist == False) || (ball1.yPos > 56 && ball2.yPos > 56 && ball3.yPos > 56 && ball4.yPos > 56 && ball5.yPos > 56 && ball6.yPos > 56 && ball7.yPos > 56) || (ball1.yPos > 56 && ball2.yPos > 56 && ball3.exist == False && ball4.exist == False && ball5.exist == False && ball6.exist == False && ball7.exist == False) || (ball1.yPos > 56 && ball2.exist == False && ball3.yPos > 56 && ball4.yPos > 56 && ball5.yPos > 56 && ball6.yPos > 56 && ball7.yPos > 56) then
            if newPlayer.lives == 0 then
                { model | state = Lose, player = newPlayer }
            else
                { model | state = LoseOneLife, player = newPlayer }
        else
            model

--Help moveBalls to choose ball or updatedBall
chooseBall: Ball -> Ball -> Bool -> Ball
chooseBall ball updatedBall bool =
    if bool then
        updatedBall
    else
        ball

--Help moveBalls to choose model or updatedModel
chooseModel: Model -> Ball -> Int -> Model
chooseModel lastUpdatedModel ball number=
    let
        upDatedModel = Tuple.first ( moveOneBall lastUpdatedModel (Tuple.first ((getBallFunction number) lastUpdatedModel "b")) )
    in
        if ball.exist == True then
            upDatedModel
        else
            lastUpdatedModel

--Show the ball to users
svgBall : Ball -> Svg msg
svgBall ball =
    image
        [ x (toString (ball.xPos - 1))
        , y (toString (ball.yPos - 1))
        , xlinkHref ball.texture
        , width "2"
        , height "2"
        ]
        []