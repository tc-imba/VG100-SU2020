module Heros exposing(teacherConfig,mN,mK,zQ,hSS,teachers,getFirstTeacher,getNextTeacher,getPreviousTeacher,Teacher,Skills(..))


type Skills
    = Bomb
    | Addline
    | ResetBall
    | Bonus


type alias Teacher =
    {
        ballSpeed : Float
    ,   batSpeed : Float
    ,   batWidth : Float
    ,   score : Float
    ,   name : String
    ,   description1 : String
    ,   description2 : String
    ,   url : String
    ,   background : String
    ,   skill : Skills
    }


teacherConfig : Teacher
teacherConfig = Teacher 1 1 1 1 "" "" "" "" "" Bomb

mN : Teacher
mN = Teacher 1 0.5 1 1.2 
    "Cyber King" 
    "Exclusive Bonus: Score * 1.2"
    "Overclocking: The score and bonus of breaking the next brick times 5"
    "./images/MN.jpg"
    "Background: Martinez Chevalier mainly teaches computer science. One of his feature is that bonus is appealing to him. He always sets some unreachable bonus for his students." Bonus

mK : Teacher
mK = Teacher 1 0.5 1.2 1 
    "The Sword of Physics" 
    "Buffer Area: Batlength * 1.2" 
    "Oppression: Add oneline to your opponents"
    "./images/MK.jpg"
    "Background: Matteo Kosacki is teaching physics. He has an excellent capacity to bear. He can bear more pressure from students than other teachers, which gives him a bigger chance to win the game." Addline

zQ : Teacher
zQ = Teacher 1 0.6 1 1 
    "Hand of Ragnaros" 
    "Turbo: Batspeed * 1.2" 
    "Inferno Bomb: Break the Bricks around your ball"
    "./images/ZQ.jpg"
    "Background: Zhao Qi is a Chinese physics teacher. He is teaching thermodynamics right now. What he is teaching provides him a body like a internal combustion engine. He can heaten his core, which makes he move faster than others." Bomb

hSS : Teacher
hSS = Teacher 1.2 0.5 1 1 
      "The Master of Space" 
      "Curvature-Driven: Ballspeed * 1.2"
      "Recursion： Reset your ball" 
      "./images/HSS.jpg"
      "Background: Helmut Heinrich is a math teacher. It seems that he concentrates on some intricate \"spaces\", which gives him a more agile mind than others. He can drive the students away more effectively." ResetBall 

teachers : List Teacher
teachers = [mN,mK,zQ,hSS]

getFirstTeacher : List Teacher -> Teacher
getFirstTeacher model = 
    let
        first = List.head model
        getT =
            case first of
                Just t ->
                    t
                Nothing ->
                    teacherConfig
    in
        getT
    

getNextTeacher : List Teacher -> List Teacher
getNextTeacher model =
    let
        firstTeacher = List.take 1 model
        teacherTail = List.drop 1 model 
        
        newTeacher = List.append teacherTail firstTeacher

    in 
        newTeacher


getPreviousTeacher : List Teacher -> List Teacher
getPreviousTeacher model =
    let
        teacherHead = List.take 3 model
        teacherTail = List.drop 3 model 

        newTeacher = List.append teacherTail teacherHead
    in
        newTeacher

