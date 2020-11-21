| package |
package := Package name: 'Cine'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #Account;
	add: #Cinema;
	add: #CinemaSystem;
	add: #Client;
	add: #Film;
	add: #Room;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: #(
	'Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin'
	'Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin Legacy Date & Time'
	'Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin Message Box'
	'Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\MVP\Presenters\Prompters\Dolphin Prompter').

package!

"Class Definitions"!

Object subclass: #Account
	instanceVariableNames: 'number titular'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Cinema
	instanceVariableNames: 'name address capacity rooms accounts billboard'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #CinemaSystem
	instanceVariableNames: 'establishment'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Client
	instanceVariableNames: 'name lastname dni'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Film
	instanceVariableNames: 'name duration'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Room
	instanceVariableNames: 'number films ticketSold totalEntries priceEntry day'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

Account guid: (GUID fromString: '{6f395d3f-f100-4f10-b966-35c943b36580}')!
Account comment: ''!
!Account categoriesForClass!Kernel-Objects! !
!Account methodsFor!

initAccount: isTitular number: isNumber
	"inicializa la cuenta"

	number := isNumber.
	titular := isTitular!

viewNumber
	^number!

viewTitular
	^titular! !
!Account categoriesFor: #initAccount:number:!public! !
!Account categoriesFor: #viewNumber!public! !
!Account categoriesFor: #viewTitular!public! !

!Account class methodsFor!

createAccount: isClient
	^self new initAccount: isClient number: isClient viewDni! !
!Account class categoriesFor: #createAccount:!public! !

Cinema guid: (GUID fromString: '{95c4e4e8-6e15-48da-bb47-0aaa5a700220}')!
Cinema comment: ''!
!Cinema categoriesForClass!Kernel-Objects! !
!Cinema methodsFor!

_availability_
	| a |
	a := rooms reject: [:tic | tic viewTicketSold = tic viewTotalEntries].!

_exit_: option
	(MessageBox confirm: 'SALIR?') ifFalse: [self _start_: option]!

_roomDay_
	| a |
	a := self searchFilm_day: (Prompter prompt: 'ingrese un dia de la semana: ').
	^a!

_sale_
	| option exit s |
	exit := 3.
	option := Dictionary new.
	s := ' 1 - comprar     2 - registrar usuario  3 - salir'.
	option at: 1 put: [self viewRooms].
	option at: 2 put: [self viewRooms].
	option at: exit put: [self _exit_: option].!

_start_: option
	| p |
	p := Prompter prompt: ' 1 - VER CARTELERA   2 - REGISTRAR  3- SALIR'.
	(p ~= 1 and: [p ~= 2]) ifTrue: [p := 3].
	(option at: p) value!

addAccount: isAccount
	accounts add: isAccount!

addRoom: isRoom
	rooms add: isRoom!

applyDiscount: isTotal
	"15% de descuento"

	^isTotal / 100 * 15!

existAccount: isDni
	"retorna un booleano"

	^(self searchAccount: isDni) isNil ifTrue: [^false] ifFalse: [^true]!

initCinema: isName address: isAddress capacity: isCapacity
	name := isName.
	address := isAddress.
	capacity := isCapacity.
	rooms := OrderedCollection new.
	accounts := OrderedCollection new.
	billboard := Dictionary new
				at: 'lunes' put: OrderedCollection new;
				at: 'martes' put: OrderedCollection new;
				at: 'miercoles' put: OrderedCollection new;
				at: 'jueves' put: OrderedCollection new;
				at: 'viernes' put: OrderedCollection new;
				at: 'sabado' put: OrderedCollection new;
				at: 'domingo' put: OrderedCollection new;
				yourself!

preload: isCinema
	"precarga salas/peliculas"

	isCinema addRoom: (Room
				createRoom: (Film createFilm: 'Back to the Future I' duration: '1:56')
				number: 1
				ticketSold: 0
				totalEntries: 100
				priceEntry: 80
				day: 'Lunes').
	isCinema addRoom: (Room
				createRoom: (Film createFilm: 'Back to the Future II' duration: '1:48')
				number: 2
				ticketSold: 0
				totalEntries: 100
				priceEntry: 80
				day: 'Martes').
	isCinema addRoom: (Room
				createRoom: (Film createFilm: 'Back to the Future III' duration: '1:59')
				number: 3
				ticketSold: 0
				totalEntries: 100
				priceEntry: 80
				day: 'Miercoles').
	isCinema addRoom: (Room
				createRoom: (Film createFilm: 'Forrest Gump' duration: '2:22')
				number: 4
				ticketSold: 0
				totalEntries: 100
				priceEntry: 80
				day: 'Jueves').

	"precarga de cuentas"
	isCinema
		addAccount: (Account createAccount: (Client
						createClient: 'Jose'
						lastName: 'Bravo'
						dni: '39876473')).
	isCinema
		addAccount: (Account createAccount: (Client
						createClient: 'Rosa'
						lastName: 'Sofia'
						dni: '39874363')).
	isCinema
		addAccount: (Account createAccount: (Client
						createClient: 'Pablo'
						lastName: 'Javier'
						dni: '98372712')).
	Transcript clear.
	Transcript
		tab: 6;
		show: 'fin de la precarga de datos del cine';
		tab: 3;
		display: Time now;
		cr!

searchAccount: isDni
	"devuelve una cuenta buscada por el dni"

	^accounts detect: [:cuenta | cuenta viewNumber = isDni] ifNone: [^nil]	"** uso de detect **"!

searchFilm: isFilm
	^rooms detect: [:e | e viewFilms = isFilm] ifNone: [^nil] inspect	"detect"!

searchFilm_day: isDay
	| data |
	data := rooms select: [:e | e viewDay = isDay].	"uso de select"
	^data!

searchFilm_duration
	| data |
	data := accounts collect: [:po | po viewTitular].
	^data inspect!

viewBillboard
	^billboard!

viewRooms
	^rooms! !
!Cinema categoriesFor: #_availability_!public! !
!Cinema categoriesFor: #_exit_:!public! !
!Cinema categoriesFor: #_roomDay_!public! !
!Cinema categoriesFor: #_sale_!public! !
!Cinema categoriesFor: #_start_:!public! !
!Cinema categoriesFor: #addAccount:!public! !
!Cinema categoriesFor: #addRoom:!public! !
!Cinema categoriesFor: #applyDiscount:!public! !
!Cinema categoriesFor: #existAccount:!public! !
!Cinema categoriesFor: #initCinema:address:capacity:!public! !
!Cinema categoriesFor: #preload:!public! !
!Cinema categoriesFor: #searchAccount:!public! !
!Cinema categoriesFor: #searchFilm:!public! !
!Cinema categoriesFor: #searchFilm_day:!public! !
!Cinema categoriesFor: #searchFilm_duration!public! !
!Cinema categoriesFor: #viewBillboard!public! !
!Cinema categoriesFor: #viewRooms!public! !

!Cinema class methodsFor!

createCinemaName: isName address: isAddress capacity: isCapacity
	^self new
		initCinema: isName
		address: isAddress
		capacity: isCapacity! !
!Cinema class categoriesFor: #createCinemaName:address:capacity:!public! !

CinemaSystem guid: (GUID fromString: '{43dad70a-4ab2-46bf-905a-c1add3621280}')!
CinemaSystem comment: ''!
!CinemaSystem categoriesForClass!Kernel-Objects! !
!CinemaSystem methodsFor!

billboard
	establishment _roomDay_!

exit:option
(MessageBox confirm: 'SALIR?') ifFalse: [self start:option] .!

iniSystem: isCinema
	| option |
	establishment := isCinema.
	self preload.
	option := Dictionary new.
	option at: 1 put: [self sale].
	option at: 2 put: [self billboard].
	option at: 3 put: [self exit: option].
	self start: option.
	self exit: option!

preload
	establishment preload: establishment!

sale
	establishment _sale_!

start: option
	| p |
	p := (Prompter prompt: 'opcion: (1) - VENTA  (2) - CARTELERA  (3) - SALIR') asNumber.
	(p ~= 1 and: [p ~= 2]) ifTrue: [p := 3].
	(option at: p) value! !
!CinemaSystem categoriesFor: #billboard!public! !
!CinemaSystem categoriesFor: #exit:!public! !
!CinemaSystem categoriesFor: #iniSystem:!public! !
!CinemaSystem categoriesFor: #preload!public! !
!CinemaSystem categoriesFor: #sale!public! !
!CinemaSystem categoriesFor: #start:!public! !

!CinemaSystem class methodsFor!

createSystem: isCinema
	^self new iniSystem: isCinema! !
!CinemaSystem class categoriesFor: #createSystem:!public! !

Client guid: (GUID fromString: '{5d681b23-0e11-4fb8-9dea-9386455a5812}')!
Client comment: ''!
!Client categoriesForClass!Kernel-Objects! !
!Client methodsFor!

initClientName: isName lastname: isLastName dni: isDni
	"inicializa el cliente"

	name := isName.
	lastname := isLastName.
	dni := isDni!

viewDni
	^dni!

viewLastName
	^lastname!

viewName
	^name! !
!Client categoriesFor: #initClientName:lastname:dni:!public! !
!Client categoriesFor: #viewDni!public! !
!Client categoriesFor: #viewLastName!public! !
!Client categoriesFor: #viewName!public! !

!Client class methodsFor!

createClient: isName lastName: isLastName dni: isDni
	"crea la pelicula"

	^self new
		initClientName: isName
		lastname: isLastName
		dni: isDni! !
!Client class categoriesFor: #createClient:lastName:dni:!public! !

Film guid: (GUID fromString: '{4bc77407-bc3d-43fd-953c-a180d20e804f}')!
Film comment: ''!
!Film categoriesForClass!Kernel-Objects! !
!Film methodsFor!

initAccountName: isName duration: isDuration
	"inicializa la pelicula"

	name := isName.
	duration := isDuration!

viewDuration
	^duration!

viewName
	^name! !
!Film categoriesFor: #initAccountName:duration:!public! !
!Film categoriesFor: #viewDuration!public! !
!Film categoriesFor: #viewName!public! !

!Film class methodsFor!

createFilm: isName duration: isDuration
	"crea la pelicula"

	^self new initAccountName: isName duration: isDuration! !
!Film class categoriesFor: #createFilm:duration:!public! !

Room guid: (GUID fromString: '{fd320110-709c-4019-83d9-2253bd5771a2}')!
Room comment: ''!
!Room categoriesForClass!Kernel-Objects! !
!Room methodsFor!

checkAvailability
	^totalEntries - ticketSold!

initRoom: isNumber films: isFilms
	"inicializa la sala"

	number := isNumber.
	films := isFilms
	!

initRoom: isNumber films: isFilms ticketSold: isTicketSold totalEntries: isTotalEntries priceEntry: isPriceEntry day: isDay
	"inicializa la sala"

	number := isNumber.
	films := isFilms.
	ticketSold := isTicketSold.
	totalEntries := isTotalEntries.
	priceEntry := isPriceEntry.
	day := isDay!

searchFilm_duration
	| data |
	data := films collect: [:po | po viewFilms].
	^data inspect!

sell: count
	ticketSold := ticketSold + count!

viewDay
	^day!

viewFilms
	^films!

viewName
	^super viewName!

viewNumber
	^number!

viewTicketSold
	^ticketSold!

viewTotalEntries
	^totalEntries!

viwPrinceEntry
	^priceEntry! !
!Room categoriesFor: #checkAvailability!public! !
!Room categoriesFor: #initRoom:films:!public! !
!Room categoriesFor: #initRoom:films:ticketSold:totalEntries:priceEntry:day:!public! !
!Room categoriesFor: #searchFilm_duration!public! !
!Room categoriesFor: #sell:!public! !
!Room categoriesFor: #viewDay!public! !
!Room categoriesFor: #viewFilms!public! !
!Room categoriesFor: #viewName!public! !
!Room categoriesFor: #viewNumber!public! !
!Room categoriesFor: #viewTicketSold!public! !
!Room categoriesFor: #viewTotalEntries!public! !
!Room categoriesFor: #viwPrinceEntry!public! !

!Room class methodsFor!

createRoom: isFilm number: isNumber
	^self new initRoom: isNumber films: isFilm!

createRoom: isFilm number: isNumber ticketSold: isTicketSold totalEntries: isTotalEntries priceEntry: isPriceEntry day: isDay
	^self new
		initRoom: isNumber
		films: isFilm
		ticketSold: isTicketSold
		totalEntries: isTotalEntries
		priceEntry: isPriceEntry
		day: isDay! !
!Room class categoriesFor: #createRoom:number:!public! !
!Room class categoriesFor: #createRoom:number:ticketSold:totalEntries:priceEntry:day:!public! !

"Binary Globals"!

