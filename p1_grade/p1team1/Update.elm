module Update exposing (update)


import Message exposing (Msg(..), Buff(..))
import Model exposing (Model, State(..),initialModel, ShoppingStatus(..), loadStage)
import Entity exposing (movePaddle, Rect, Circle, CircleType(..), Paddle, Block, BlockType(..), blockAfterHit, valueOfBlock, coinsFromList, movePaddle_, bonusFromList)
import Random exposing (Seed, float, step)
import Color exposing (fromName)

update: Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
        Resize width height ->
            ( { model | size = ( toFloat width, toFloat height ) }
            , Cmd.none
            )
        GetViewport { viewport } ->
            ( { model
                | size =
                    ( viewport.width
                    , viewport.height
                    )
              }
            , Cmd.none
            )
        MoveRight ->
            if model.status == Playing then
                let
                    newPaddle = movePaddle False model.fasterPaddle model.width model.paddle
                    newModel = {model|paddle=newPaddle}
                in
                (newModel,Cmd.none)
            else
                (model, Cmd.none)
        MoveLeft ->
            if model.status == Playing then
                let
                    newPaddle = movePaddle True model.fasterPaddle model.width model.paddle
                    newModel = {model|paddle=newPaddle}
                in  
                (newModel,Cmd.none)
            else
                (model, Cmd.none)
        MoveUp ->
            if model.status == Playing then
                let
                    newPaddle = movePaddle_ True model.fasterPaddle model.height model.paddle
                    newModel = {model|paddle=newPaddle}
                in
                (newModel,Cmd.none)
            else
                (model, Cmd.none)
        MoveDown ->
            if model.status == Playing then
                let
                    newPaddle = movePaddle_ False model.fasterPaddle model.height model.paddle
                    newModel = {model|paddle=newPaddle}
                in
                (newModel,Cmd.none)
            else
                (model, Cmd.none)
        Tick deltaTime->
            if model.status == Playing then
                let
                    (collectScore, _, coinsLeft) = collectBonus model.coins
                    (_, collectedBonus, bonusLeft) = collectBonus model.bonusBalls
                    newBalls = generateBalls collectedBonus
                    newBonus = generateBonus model.blocks
                    newCoins = generateCoins model.blocks
                    (deltaScore, newBlocks) = removeDestoryedBlocks model.blocks
                    scoreRate = if model.moreScore then 1.5 else 1.0
                    newModel = 
                        { model | blocks = newBlocks
                        , coins = coinsLeft ++ newCoins
                        , score = model.score + floor( toFloat (collectScore + deltaScore) * scoreRate)
                        , bonusBalls = bonusLeft ++ newBonus
                        , balls = model.balls ++ newBalls}
                in
                (eliminate newModel deltaTime, Cmd.none)
            else
                (model, Cmd.none)
        NextStage ->
            let
                newStage = model.stage + 1
                newModel = loadStage newStage model
                newScores = newStage * newStage * 100 // 2 + 50
            in
            ( {newModel|status = Stopped, hasStarted = False, stage = newStage, requiredScore = newScores}, Cmd.none)
        Restart ->
            let
                model2 = loadStage model.stage initialModel
            in
            ( {model2|status = Stopped, hasStarted = False}, Cmd.none )
        Start ->
            let
                model2 =
                    if model.life < 3 then
                        model
                    else
                        loadStage model.stage model
            in
            ( {model2|status = Playing,hasStarted = True}, Cmd.none )
        Pause ->
            ( {model|status = Paused}, Cmd.none )
        Resume ->
            ( {model|status = Playing}, Cmd.none )
        OpenManual ->
            ( {model|status = Manual model.status}, Cmd.none )
        OpenGamePage ->
            if model.hasStarted then
                ({model|status = Paused}, Cmd.none)
            else
                ({model|status = Stopped}, Cmd.none)
        OpenFirstPage ->
            ({model|status = FirstPage}, Cmd.none)
        OpenReference ->
            ( {model|status = Reference model.status}, Cmd.none )
        ArriveStore ->
            ( {model|status = Store model.status}, Cmd.none )
        LeaveStore ->
            let
                status = case model.status of
                    Store lastStatus ->
                        lastStatus
                    _ -> Paused
            in
            ( {model|status = status, shopping = Fine}, Cmd.none )
        BuyBuff buff ->
            case buff of
                FasterPaddle ->
                    if model.score > 75 then
                        if model.fasterPaddle then
                            ({model|shopping=Twice},Cmd.none)
                        else
                            ({model|fasterPaddle=True, score = model.score - 75, shopping=Buy buff},Cmd.none)
                    else
                        ({model|shopping=Poor},Cmd.none)
                LongPaddle ->
                    if model.score > 75 then
                        if model.longPaddle then
                            ({model|shopping=Twice},Cmd.none)
                        else
                            ({model|longPaddle=True, score = model.score - 75, shopping=Buy buff},Cmd.none)
                    else
                        ({model|shopping=Poor},Cmd.none)
                MoreScore ->
                    if model.score > 150 then
                        if model.moreScore then
                            ({model|shopping=Twice},Cmd.none)
                        else
                            ({model|moreScore=True, score = model.score - 150, shopping=Buy buff},Cmd.none)
                    else
                        ({model|shopping=Poor},Cmd.none)
                MoreLife ->
                    if model.score > 150 then
                        ({model|life = model.life + 1, score = model.score - 150, shopping=Buy buff},Cmd.none)
                    else
                        ({model|shopping=Poor},Cmd.none)
        NoOp ->
            (model, Cmd.none)

removeDestoryedBlocks : List Block -> (Int, List Block)
removeDestoryedBlocks list =
    let 
        (destroyedBlocks, restBlocks) = List.partition (\block -> block.isDestroyed) list
        simpleScore = destroyedBlocks
            |> List.filter (\block -> block.category == SimpleBlock)
            |> List.length
            |> (\k -> k * 5)
        strongScore = destroyedBlocks
            |> List.map 
                ( \block ->
                    case block.category of 
                        StrongBlock str ->
                            str * 5
                        _ -> 0
                )
            |> List.sum
        score = simpleScore + strongScore
    in
    (score, restBlocks)

-- generate the balls when the bonus balls collide with the paddle
generateBalls : List Circle -> List Circle
generateBalls bonus =
    case bonus of
        [] ->
            []
        head :: rest ->
            let
                ball1 = {head | radius = 15, pos = (Tuple.first head.pos - 20, Tuple.second head.pos), velocityX = -100, velocityY = -100, category = Ball, color = Color.fromName "green"}
                ball2 = {head | radius = 15, velocityX = 0, velocityY = -200, category = Ball, color = Color.fromName "green"}
                ball3 = {head | radius = 15, pos = (Tuple.first head.pos + 20, Tuple.second head.pos), velocityX = 100, velocityY = -100, category = Ball, color = Color.fromName "green"}
                bonus_ = generateBalls rest
            in
                ball1 :: ball2 :: ball3 :: bonus_

-- collect both coins and bonus balls
collectBonus : List Circle ->  (Int, List Circle, List Circle)
collectBonus bonus =
    let
        (collectedBonus, restBonus) = List.partition
            (\coin ->
            case coin.category of
                Coin isCollected _ ->
                    isCollected
                Bonus isCollected ->
                    isCollected
                _->
                    False
            )
            bonus
        valList = List.map
            (\coin ->
            case coin.category of
                Coin _ val ->
                    val
                _->
                    0
            )
            collectedBonus
        totalVal = List.sum valList

    in
    (totalVal, collectedBonus, restBonus)


generateCoins : List Block -> List Circle
generateCoins blocks =
    let
        brokenTreasureBlocks = List.filter
            (\block ->
                (case block.category of
                    TreasureBlock _ ->
                        True
                    _ -> False
                )
                && block.isDestroyed )
            blocks
        posList = List.map (\block -> block.rect.centerPos) brokenTreasureBlocks
        valueList = List.map valueOfBlock brokenTreasureBlocks
    in
        coinsFromList posList valueList

generateBonus : List Block -> List Circle
generateBonus blocks =
    let
        brokenTreasureBlocks = List.filter
            (\block ->
                (case block.category of
                    BallBlock ->
                        True
                    _ -> False
                )
                && block.isDestroyed )
            blocks
        posList = List.map (\block -> block.rect.centerPos) brokenTreasureBlocks
    in
        bonusFromList posList

-- eliminate the entities in the game world in a short time delta
eliminate : Model -> Float -> Model
eliminate model elapsed =
    let
        activeBalls = List.filter (\ball -> floor (Tuple.second ball.pos - ball.radius) < model.height) model.balls
    in
    if List.length activeBalls == 0 then
        if model.life == 1 then
            -- Game over
            gameOver model
        else
            survive model
    else
        let
            -- move the ball
            originalBalls = activeBalls
            --(x, y) = originalBall.pos
            --movedBalls = {originalBall | pos = (x + elapsed * originalBall.velocityX / 1000, y + elapsed * originalBall.velocityY / 1000)}
            -- the collide judgement for the ball
            --(seed1, ball1) = collideRect model.seed ballAfterHit model.paddle.rect movedBall
            movedBalls = moveBalls elapsed originalBalls
            (seed0, updatedBalls) = updateBalls model.seed movedBalls
            (seed1, ball1) = collidePaddle seed0 model.paddle.rect updatedBalls
            ball2 =  collideWall_ model.width ball1
            (seed2, newBlocks, ball3) = updateBlocks_ seed1 model.width model.blocks ball2
            -- move the coins
            movedCoins = List.map
                (\c ->
                    {c | pos = (Tuple.first c.pos + elapsed * c.velocityX / 1000, Tuple.second c.pos + elapsed * c.velocityY / 1000)}
                )
                model.coins
            movedBonus = List.map
                (\c ->
                    {c | pos = (Tuple.first c.pos + elapsed * c.velocityX / 1000, Tuple.second c.pos + elapsed * c.velocityY / 1000)}
                )
                model.bonusBalls
            (seed3, updatedCoins) = updateTreasure seed2 bonusAfterHit model.paddle.rect movedCoins
            (seed4, updatedBonus) = updateTreasure seed3 bonusAfterHit model.paddle.rect movedBonus
            paddle = model.paddle
            newPaddle = if model.longPaddle then longerPaddle paddle else paddle
        in
        {model | balls = ball3, blocks = newBlocks, seed = seed4, coins = updatedCoins, paddle = newPaddle, longPaddle = False, bonusBalls = updatedBonus}

moveBalls : Float -> List Circle -> List Circle
moveBalls elapsed balls =
    case balls of
        [] ->
            []
        head :: rest ->
            let
                (x, y) = head.pos
                head_ = {head | pos = (x + elapsed * head.velocityX / 1000, y + elapsed * head.velocityY / 1000)}
                balls_ = moveBalls elapsed rest
            in
                head_ :: balls_

-- list of balls collide with the wall
collideWall_ : Int -> List Circle -> List Circle
collideWall_ wid balls =
    case balls of
        [] ->
            []
        head :: rest ->
            let
                head_ = collideWall wid head
                updatedRest = collideWall_ wid rest
            in
                head_ :: updatedRest

-- one ball collides with the wall
collideWall : Int -> Circle -> Circle
collideWall wid sample =
    let
        (x, y) = sample.pos
        (vx, vy) = (sample.velocityX, sample.velocityY)
    in
        if x -  sample.radius <= 0 then
        -- Left wall
            let
                newX =  sample.radius + 0.1
                newBall = {sample | pos = (newX , y), velocityX = -vx}
            in
                newBall
        else if x + sample.radius >= toFloat wid then
        -- Right wall
            let
                newX =   toFloat wid - sample.radius + 0.1
                newBall = {sample | pos = (newX, y), velocityX = -vx}
            in
                newBall
        else if y - sample.radius <= 0 then
            let
                dy = sample.radius - y + 0.1
                newBall = {sample | pos = (x, y + dy), velocityY = -vy}
            in
            newBall
        else
            sample

type alias CircleUpdater = (Seed -> Rect -> Circle -> (Seed, Circle))

-- list of balls collide with the paddle
collidePaddle : Seed -> Rect -> List Circle -> (Seed, List Circle)
collidePaddle seed rect balls =
    case balls of
        [] ->
            (seed, balls)
        head :: rest ->
            let
                (newSeed, head_) = collideRect seed ballAfterHit rect head
                (updatedSeed, balls_) = collidePaddle newSeed rect rest
            in
                (updatedSeed, head_ :: balls_)

collideRect : Seed -> CircleUpdater -> Rect -> Circle -> (Seed, Circle)
collideRect seed fun rect sample =
    let
        (x, y) = rect.centerPos
        (xX, yY) = sample.pos
        relativeX = xX - x
        relativeY = yY - y
        dx = clamp -(toFloat rect.width / 2) (toFloat rect.width / 2) relativeX  
        dy = clamp -(toFloat rect.height / 2) (toFloat rect.height / 2) relativeY
        is_hit = (dx - relativeX) ^ 2 + (dy - relativeY) ^ 2 < sample.radius ^ 2
    in
    if is_hit then
        fun seed rect sample
    else
        (seed, sample)

        
-- update the blocks that are collided by list of balls
updateBlocks_ : Seed -> Int -> List Block -> List Circle -> (Seed, List Block, List Circle)
updateBlocks_ seed wid blocks balls =
    case balls of
        [] ->
            (seed, blocks, [])
        head :: rest ->
            let
                (newSeed, newBlocks, head_) = updateBlocks seed wid blocks head
                (updatedSeed, newBlocks_, balls_) = updateBlocks_ newSeed wid newBlocks rest
            in
                (updatedSeed, newBlocks_, head_ :: balls_)


-- update the blocks that are collided by one ball
updateBlocks : Seed -> Int -> List Block -> Circle -> (Seed, List Block, Circle)
updateBlocks seed wid blocks sample =
    case blocks of
        [] ->
            (seed, [], sample)
        block :: rest ->
            let
                (newSeed, newBall) = collideRect seed ballAfterHit block.rect sample
                (updatedSeed, updatedRest,restBall) = updateBlocks newSeed wid rest sample
            in
            if sample /= newBall then
                (updatedSeed, blockAfterHit block :: updatedRest, newBall) 
            else
                (updatedSeed, block :: updatedRest, restBall) 

updateTreasure : Seed -> CircleUpdater -> Rect -> List Circle -> (Seed, List Circle)
updateTreasure seed fun paddle coins =
    case coins of 
        [] ->
            (seed, [])
        head :: rest ->
            let 
                (newSeed, newCoin) = collideRect seed fun paddle head
                (updatedSeed, rest_) = updateTreasure newSeed fun paddle rest
            in
            (updatedSeed, newCoin :: rest_)


sgn : number -> number
sgn x = 
    if x > 0 then
        1
    else if x == 0 then
        0
    else
        -1

ballAfterHit : CircleUpdater
ballAfterHit seed rect sample =
    let
        (x, y) = rect.centerPos
        (xX, yY) = sample.pos
        relativeX = xX - x
        relativeY = yY - y
        dx = clamp -(toFloat rect.width / 2) (toFloat rect.width / 2) relativeX  
        dy = clamp -(toFloat rect.height / 2) (toFloat rect.height / 2) relativeY
        (randomDelta, newSeed) = step (Random.float -0.1 0.1) seed
        randomRate = randomDelta + 1
        speed = sqrt (sample.velocityX ^ 2 + sample.velocityY ^ 2)
    in
    -- if  dx > x -  toFloat rect.width / 2 && dx < x + toFloat rect.width / 2 then
    if  dx == relativeX then
        -- hit the top/ bottom of a rect
        let
            dY =
                if relativeY - dy < 0 then
                -- top
                    sample.radius + relativeY - dy
                else
                -- bottom
                    -sample.radius + relativeY - dy
            vY =
                if relativeY - dy < 0 then
                    -(abs sample.velocityY)
                else
                    abs sample.velocityY
            vYRandom = clamp -(speed - 40) (speed - 40) (randomRate * vY)
            newBall = {sample | pos = (xX, yY - dY), velocityY = vYRandom, velocityX = sgn sample.velocityX * sqrt(speed ^ 2 - vYRandom ^ 2) }
        in
            (newSeed, newBall)
    else
        let
            dX =
                if relativeX - dx > 0 then
                    sample.radius - relativeX + dx
                else
                    -sample.radius - relativeX + dx
            vX =
                if relativeX - dx > 0 then
                    abs sample.velocityX
                else
                    -(abs sample.velocityX)
            vXRandom =  clamp -(speed - 40) (speed - 40) (vX * randomRate)
            newBall = {sample | pos = (xX + dX, yY), velocityX = vXRandom, velocityY = sgn sample.velocityY * sqrt(speed ^ 2 - vXRandom ^ 2)}
        in
        (newSeed, newBall)

bonusAfterHit: CircleUpdater
bonusAfterHit seed _ treasure =
    case treasure.category of
        Coin _ value ->
            (seed, {treasure| category = Coin True value})
        Bonus _ ->
            (seed, {treasure | category = Bonus True})
        _ ->
            -- This should not happen
            (seed, treasure)


-- ball collides with each other
collideBall: Seed -> Circle -> Circle -> (Seed, (Circle, Circle))
collideBall seed ball1 ball2 =
    let
        (x1, y1) = ball1.pos
        (x2, y2) = ball2.pos
        dx = x1 - x2
        dy = y1 - y2
        isHit = dx ^ 2 + dy ^ 2 < (ball1.radius + ball2.radius) ^ 2
    in
        if isHit then
            updatedBall seed ball1 ball2
        else
            (seed, (ball1, ball2))


updatedBall: Seed -> Circle ->  Circle -> (Seed, (Circle, Circle))
updatedBall seed ball1 ball2 =
    let
        (x1, y1) = ball1.pos
        (x2, y2) = ball2.pos
        (r1, r2) = (ball1.radius, ball2.radius)
        dx = x1 - x2
        dy = y1 - y2
        d = sqrt (dx ^ 2 + dy ^ 2)
        k =
            if dy /= 0 then
                -dx / dy
            else 9999999
        x1_ = Tuple.first ball1.pos + (r1 + r2 - d) *  dx / d
        y1_ = Tuple.second ball1.pos + (r1 + r2 - d) *  dy / d
        -- in case of bug, move the ball from another ball
        ball1_ = {ball1 | pos = (x1_, y1_)}
        (vx1, vy1) = (ball1.velocityX, ball1.velocityY)
        (vx2, vy2) = (ball2.velocityX, ball2.velocityY)
        vY1 = (2 * vx1 * k + k ^ 2 * vy1 - vy1) / (k ^ 2 + 1)
        vX1 = vx1 + vy1 * k - vY1 * k
        vY2 = (2 * vx2 * k + k ^ 2 * vy2 - vy2) / (k ^ 2 + 1)
        vX2 = vx2 + vy2 * k - vY2 * k
        newBall1 = {ball1_ | velocityX = vX1, velocityY = vY1}
        newBall2 = {ball2 | velocityX = vX2, velocityY = vY2}
    in
        (seed, (newBall1, newBall2))


-- whether a ball hits other balls
ballAgainstBalls: Seed -> Circle -> List Circle -> (Seed, Circle, List Circle)
ballAgainstBalls seed sample balls =
    case balls of
        [] ->
            (seed, sample,[])
        head :: rest ->
            let
                (newSeed, (ball1, ball2)) = collideBall seed sample head
                (updatedSeed_, ball1_, updatedBalls) = ballAgainstBalls newSeed ball1 rest
            in
                (updatedSeed_, ball1_, ball2 :: updatedBalls)

-- update all balls
updateBalls: Seed -> List Circle -> (Seed, List Circle)
updateBalls seed balls =
    case balls of
        [] ->
            (seed, [])
        head :: rest ->
            let
                (newSeed, head_, updatedBalls) = ballAgainstBalls seed head rest
                (updatedSeed, newBalls) = updateBalls newSeed updatedBalls
            in
                (updatedSeed, head_ :: newBalls)


gameOver: Model -> Model
gameOver model =
    {initialModel| size = model.size, seed = model.seed, status = Again}

survive : Model -> Model
survive model =
    let 
        paddle = model.paddle
        rect = paddle.rect
        newRect = {rect | centerPos = model.initialPaddle.rect.centerPos}
        newPaddle = {paddle|rect = newRect}
    in
    {model | balls = model.initialBalls, life = model.life - 1, status = Stopped, paddle = newPaddle}

longerPaddle : Paddle -> Paddle
longerPaddle paddle =
    let
        rect = paddle.rect
        newRect = {rect|width = 3 * rect.width // 2, height = 3 * rect.height // 2}
    in
    {paddle | rect = newRect}

