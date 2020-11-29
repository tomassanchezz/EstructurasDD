module Base.EscuelaDeProgramacion(
  EscuelaDeProgramacion,
  cantidadAlumnos,
  agregarAlumno,
  agregarMateria,
  fundarEscuela
) 
where 

type Legajo = Int
type Nombre = String
type Materia = String
data Alumno = ElAlumno 
  Legajo 
  Nombre 
  [Materia] -- aprobadas
  [Materia] -- en curso
  deriving Show

-- inv rep: no hay 2 alumnos con el mismo legajo
-- un alumno no puede cursar una materia que no este en la curricula
-- no puede haber 2 materias con el mismo nombre en la curricula
-- no puede haber repetidos en las materias aprobadas de un Alumno
-- no puede haber repetidos en las materias en curso de un Alumno
data EscuelaDeProgramacion = LaEscuela [Alumno] [Materia] deriving Show

-- tpi = LaEscuela [] ["EstructurasDeDatos", "EstructurasDeDatos"]

cantidadAlumnos :: EscuelaDeProgramacion -> Int 
cantidadAlumnos (LaEscuela alumnos materias) = length alumnos

fundarEscuela :: EscuelaDeProgramacion
fundarEscuela = LaEscuela [] []

agregarAlumno :: Nombre -> EscuelaDeProgramacion -> EscuelaDeProgramacion
agregarAlumno nom (LaEscuela alumnos materias) =
  LaEscuela (agregarAlumno' nom alumnos) materias

agregarAlumno' :: Nombre -> [Alumno] -> [Alumno]
agregarAlumno' nom [] = [ElAlumno 0 nom [] []]

agregarMateria :: Materia -> EscuelaDeProgramacion -> EscuelaDeProgramacion
agregarMateria mat (LaEscuela alumnos materias) =
  if elem mat materias
    then error "esta materia ya existe en la curricula"
    else LaEscuela alumnos (mat:materias)













