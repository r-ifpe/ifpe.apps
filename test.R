# teste sistec
ciclo <- read_sistec_ciclo("inst/ciclo/")
students <- read_sistec_students("inst/students/")
d <- join_students_ciclo(students, ciclo)

# teste qacademico
con <- connect_qacademico("inst/credentials/")
a <- read_qacademico_students(con)

