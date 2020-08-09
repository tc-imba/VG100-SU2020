module Skill exposing (..)

import Model exposing (..)

--Get additional skills according to the scores
scoreWrtDiff : Model -> Int
scoreWrtDiff model =
    case model.difficulty of
        Easy ->
            300
        Medium ->
            400
        Difficult ->
            500

--Get additional skills according to the scores
scoreSkill: Model -> Model
scoreSkill model =
    let
        player = model.player
        newAddSkill = player.addSkill - scoreWrtDiff model
        newPlayer = {player | addSkill = newAddSkill}
    in
        if model.player.addSkill >= scoreWrtDiff model then
            {model | player = newPlayer, numberOfSkills = model.numberOfSkills +1 }
        else
            model


--How many skills you can get initially
skillWrtDiff : Model -> Int
skillWrtDiff model =
    case model.difficulty of
        Easy ->
            2
        Medium ->
            3
        Difficult ->
            4

--Skill1 (use `A`) Mighty Cannon: Five addition balls will occur!
skillFiveBall: Model -> Model
skillFiveBall model =
    let
        ball1 = Tuple.first (oneBall model "b")
        ball2 = Tuple.first (twoBall model "b")
        ball3 = Tuple.first (threeBall model "b")
        ball4 = Tuple.first (fourBall model "b")
        ball5 = Tuple.first (fiveBall model "b")
        ball6 = Tuple.first (sixBall model "b")
        ball7 = Tuple.first (sevenBall model "b")

        v = sqrt(2) * model.diffClass.ballSpeed

        newBall3 = {ball3 | exist = True, xPos = 27.5, yPos = 50, dx = -1 * sqrt(3) * v / 2, dy = -1 * v / 2}
        newBall4 = {ball4 | exist = True, xPos = 32.5, yPos = 50, dx = -1 * v / sqrt(2), dy = -1 * v / sqrt(2)}
        newBall5 = {ball5 | exist = True, xPos = 37.5, yPos = 50, dx = 0, dy = -1 * v}
        newBall6 = {ball6 | exist = True, xPos = 42.5, yPos = 50, dx = v / sqrt(2), dy = -1 * v / sqrt(2)}
        newBall7 = {ball7 | exist = True, xPos = 47.5, yPos = 50, dx = sqrt(3) * v / 2, dy = -1 * v / 2}

        judgeSkillAvailable1 = ball1.yPos <= 57 && ball2.yPos > 57 && ball3.yPos > 57 && ball4.yPos > 57 && ball5.yPos > 57 && ball6.yPos > 57 && ball7.yPos > 57
        judgeSkillAvailable2 = ball1.yPos <= 57 && ball2.yPos <= 57 && ball3.yPos > 57 && ball4.yPos > 57 && ball5.yPos > 57 && ball6.yPos > 57 && ball7.yPos > 57
    in
        if model.numberOfSkills > 0 && model.typeOfSkills == FiveBall && model.useSkill == True && (judgeSkillAvailable1 || judgeSkillAvailable2) then
            {model | balls = [ball1, ball2, newBall3, newBall4, newBall5, newBall6, newBall7], useSkill = False, numberOfSkills = model.numberOfSkills - 1 }
        else
            {model | useSkill = False}

--Skill2 (use `S`) Insurance: make your paddle longer
skillLongPaddle: Model -> Model
skillLongPaddle model =
    let
        paddle = model.paddle
        newWidth = paddle.width + 2
        newPaddle = {paddle | width = newWidth}
    in
        if model.numberOfSkills > 0 && model.typeOfSkills == LongPaddle && model.useSkill == True && model.paddle.width <= 17 then
            {model | paddle = newPaddle, numberOfSkills = model.numberOfSkills - 1, useSkill = False}
        else
            {model | useSkill = False}

--Skill3 (use `D`) Revive: lives = lives + 1
skillLongLife: Model -> Model
skillLongLife model =
    let
        player = model.player
        newLives = player.lives + 1
        newPlayer = {player | lives = newLives}
    in
        if model.numberOfSkills > 0 && model.typeOfSkills == LongLife && model.useSkill == True then
            {model | player = newPlayer, numberOfSkills = model.numberOfSkills - 1, useSkill = False}
        else
            {model | useSkill = False}

--Organize the three skills and use them
useSkill: Model -> Model
useSkill model =
    case model.typeOfSkills of
        FiveBall ->
            skillFiveBall model
        LongPaddle ->
            skillLongPaddle model
        LongLife ->
            skillLongLife model