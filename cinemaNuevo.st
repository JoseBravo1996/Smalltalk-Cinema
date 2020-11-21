"*************************************************************************************************"
"Declaracion de variables"
|cinema col col1|

"Inicializa instancia de Cine"
cinema:=Cinema createCinemaName: 'Hoyts' address: 'Calchaqui 2000' capacity: 100.

"se le asigna un cine al sistemaDeVentas"
CinemaSystem createSystem: cinema.

"*************************************************************************************************"
|cinema acc film film2 rm rm2|

"Inicializa instancia de Cine"
cinema:=Cinema createCinemaName: 'Cine UNAJ' address: 'Avenida calchaqui 1000' capacity: 400.

"Seccion Cliente"
acc:= Account createAccount: (Client
						createClient: 'Jose'
						lastName: 'Bravo'
						dni: '39876473').

"Seccion Cine"
film:= Film createFilm: 'Back to the Future'  duration: '1:30'.
film2:= Film createFilm: 'Back to the Future 2' duration: '1:30'.

"Sala"
rm:= Room createRoom: film  number: '1:30' ticketSold: 0 totalEntries: 100 priceEntry: 80 day: 'domingo'.
rm2:= Room createRoom: film2 number: '1:30' ticketSold: 0 totalEntries: 100 priceEntry: 80 day: 'lunes'.

cinema addRoom: rm.
cinema addRoom: rm2.
cinema addAccount: acc.

cinema searchFilm_duration.
cinema searchFilm_day: 'lunes'


