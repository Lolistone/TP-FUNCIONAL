import Test.HUnit
import Solucion

main = runTestTT tests
-- runAll = runTestTT testAll 
run1 = runTestTT testNombresDeUsuarios
run2 = runTestTT testAmigosDe
run3 = runTestTT testCantidadDeAmigos
run4 = runTestTT testUsuarioConMasAmigos 
run5 = runTestTT testEstaRobertoCarlos
run6 = runTestTT testPublicacionesDe

tests = test [
    " nombresDeUsuarios 1" ~: (nombresDeUsuarios redA) ~?= ["Juan","Natalia","Pedro","Mariela"],

    " amigosDe 1" ~: (amigosDe redA usuario1) ~?= [usuario2, usuario4],

    " cantidadDeAmigos 1" ~: (cantidadDeAmigos redA usuario1) ~?= 2,

    " usuarioConMasAmigos 1" ~: expectAny (usuarioConMasAmigos redA) [usuario2, usuario4],

    " estaRobertoCarlos 1" ~: (estaRobertoCarlos redA) ~?= False,

    " publicacionesDe 1" ~: (publicacionesDe redA usuario2) ~?= [publicacion2_1, publicacion2_2],

    " publicacionesQueLeGustanA 1" ~: (publicacionesQueLeGustanA redA usuario1) ~?= [publicacion2_2, publicacion4_1],

    " lesGustanLasMismasPublicaciones 2" ~: (lesGustanLasMismasPublicaciones redB usuario1 usuario3) ~?= True,

    " tieneUnSeguidorFiel 1" ~: (tieneUnSeguidorFiel redA usuario1) ~?= True,

    " existeSecuenciaDeAmigos 1" ~: (existeSecuenciaDeAmigos redA usuario1 usuario3) ~?= True
 ]

-- Nuestros test suite para cada ejercicio.

testNombresDeUsuarios = test [
    "Caso 1: un solo usuario" ~: (nombresDeUsuarios redUnica) ~?= ["Juan"],
    "Caso 2: usuarios disintos" ~: (nombresDeUsuarios redA) ~?= ["Juan","Natalia","Pedro","Mariela"],
    "Caso 3: usuarios repetidos" ~: (nombresDeUsuarios redB) ~?= ["Juan", "Pedro", "Natalia"],
    "Caso 4: sin usuarios" ~: (nombresDeUsuarios redVacia) ~?= []
 ]

testAmigosDe = test [
    "Caso 1: un solo usuario" ~: (amigosDe redUnica usuario1) ~?= [],
    "Caso 2: dos usuarios relacionados" ~: (amigosDe redC usuario1) ~?= [usuario2],
    "Caso 3: dos usuarios sin relacion" ~: (amigosDe redD usuario1) ~?= [],
    "Caso 4: varios usuarios, sin relacion" ~: (amigosDe redB usuario5) ~?= [],
    "Caso 5: varios usuarios, varias relaciones" ~: (amigosDe redA usuario2) ~?= [usuario1, usuario3, usuario4],
    "Caso 6: varios usuarios, unica relacion" ~: (amigosDe redB usuario1) ~?= [usuario2]
 ]

testCantidadDeAmigos = test [
    "Caso 1: el usuario no tiene amigos" ~: (cantidadDeAmigos redB usuario5) ~?= 0,
    "Caso 2: el usuario tiene 1 amigo" ~: (cantidadDeAmigos redB usuario1) ~?= 1,
    "Caso 3: el usuario tiene varios amigos" ~: (cantidadDeAmigos redA usuario2) ~?= 3
 ]

testUsuarioConMasAmigos = test [
    "Caso 1: un solo usuario" ~: (usuarioConMasAmigos redUnica) ~?= usuario1,
    "Caso 2: varios usuarios, uno con mas que el resto" ~: (usuarioConMasAmigos redB) ~?= usuario2,
    "caso 3: varios usuarios, misma cantidad amigos" ~: expectAny (usuarioConMasAmigos redA) [usuario2, usuario4] 
 ]

testEstaRobertoCarlos = test [
    "Caso 1: sin usuarios" ~: (estaRobertoCarlos redVacia) ~?= False,
    "Caso 2: un usuario" ~: (estaRobertoCarlos redUnica) ~?= False,
    "Caso 3: varios usuarios, todos menos de 10 amigos" ~: (estaRobertoCarlos redA) ~?= False,
    "Caso 4: varios usuarios, esta Roberto Carlos!" ~: (estaRobertoCarlos redRoberto) ~?= True
 ]

testPublicacionesDe = test [
    "Caso 1: ninguna publicacion" ~: (publicacionesDe redB usuario2) ~?= [],
    "Caso 2: 1 publicacion" ~: (publicacionesDe redRoberto usuario10) ~?= [publicacion10_1],
    "Caso 3: mas de una publicacion" ~: (publicacionesDe redA usuario1) ~?= [publicacion1_1, publicacion1_2]
 ]

testPublicacionesQueLeGustanA = test [

 ]

--testLesGustanLasMismasPublicaciones = test []

--testTieneUnSeguidorFiel = test []

--testExisteSecuenciaDeAmigos = test []

--testAll = test [testNombresDeUsuarios, testAmigosDe, testCantidadDeAmigos, testUsuarioConMasAmigos, testEstaRobertoCarlos, testPublicacionesDe,
--                testPublicacionesQueLeGustanA, testLesGustanLasMismasPublicaciones, testTieneUnSeguidorFiel, testExisteSecuenciaDeAmigos, tests]

expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)

-- Ejemplos

usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")
usuario6 = (6, "Lorenzo")
usuario7 = (7, "Lisandro")
usuario8 = (8, "Tiago")
usuario9 = (9, "Felipe")
usuario10 = (10, "Messi")
usuario11 = (11, "Roberto Carlos")
usuario12 = (12, "Carlitos")

relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) -- Notar que el orden en el que aparecen los usuarios es indistinto
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)
relacion11_1 = (usuario11, usuario1) 
relacion11_2 = (usuario11, usuario2)
relacion11_3 = (usuario11, usuario3)
relacion11_4 = (usuario11, usuario4)
relacion11_5 = (usuario11, usuario5)
relacion11_6 = (usuario11, usuario6)
relacion11_7 = (usuario11, usuario7)
relacion11_8 = (usuario11, usuario8)
relacion11_9 = (usuario11, usuario9)
relacion11_10 = (usuario11, usuario10)
relacion11_12 = (usuario11, usuario12)

publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2, usuario4])
publicacion1_2 = (usuario1, "Este es mi segundo post", [usuario4])
publicacion1_3 = (usuario1, "Este es mi tercer post", [usuario2, usuario5])
publicacion1_4 = (usuario1, "Este es mi cuarto post", [])
publicacion1_5 = (usuario1, "Este es como mi quinto post", [usuario5])

publicacion2_1 = (usuario2, "Hello World", [usuario4])
publicacion2_2 = (usuario2, "Good Bye World", [usuario1, usuario4])

publicacion3_1 = (usuario3, "Lorem Ipsum", [])
publicacion3_2 = (usuario3, "dolor sit amet", [usuario2])
publicacion3_3 = (usuario3, "consectetur adipiscing elit", [usuario2, usuario5])

publicacion4_1 = (usuario4, "I am Alice. Not", [usuario1, usuario2])
publicacion4_2 = (usuario4, "I am Bob", [])
publicacion4_3 = (usuario4, "Just kidding, i am Mariela", [usuario1, usuario3])

publicacion10_1 = (usuario10, "gracias al fulbo", [usuario6, usuario7])

usuariosA = [usuario1, usuario2, usuario3, usuario4]
relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4]
publicacionesA = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2]
redA = (usuariosA, relacionesA, publicacionesA)

usuariosB = [usuario1, usuario2, usuario3, usuario5]
relacionesB = [relacion1_2, relacion2_3]
publicacionesB = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redB = (usuariosB, relacionesB, publicacionesB)

usuariosC = [usuario1, usuario2]
relacionesC = [relacion1_2]
publicacionesC = []
redC = (usuariosC, relacionesC, publicacionesC)

usuariosD = [usuario1, usuario2]
relacionesD = []
publicacionesD = []
redD = (usuariosD, relacionesD, publicacionesD)

usuariosR = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9, usuario10, usuario11]
relacionesR = [relacion11_1, relacion11_2, relacion11_3, relacion11_4, relacion11_5, relacion11_6, relacion11_7, relacion11_8, relacion11_9,
               relacion11_10, relacion11_12]
publicacionesR = [publicacion10_1]
redRoberto = (usuariosR, relacionesR, publicacionesR)

usuariosU = [usuario1]
relacionesU = []
publicacionesU = []
redUnica = (usuariosU, relacionesU, publicacionesU)

redVacia = ([], [], [])