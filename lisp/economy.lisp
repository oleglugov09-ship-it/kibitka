;;;; The numbers of the game in one place: prices, pay, stock, speeds, how much fits where. Changing the
;;;; balance? This is the place (and it works live from the REPL: these are plain parameters). What they mean
;;;; in the game: see the files that use them, city.lisp (tank, gas stations), trade.lisp (clients, parcels),
;;;; interior.lisp, byt.lisp, walk.lisp.

(in-package #:kiosk-trip)

;;; Gasoline and gas stations

(defparameter *tank-km* 8.0 "The number of kilometres a full tank lasts.")
(defparameter *tank-price* 48 "The price in rubles of a full tank: 6 rubles per kilometre of road.")
(defparameter *fuel-chunk* 10 "The number of rubles of fuel poured per press of R.")
(defparameter *plank-price* '(3 5) "The gas station plank deal: how many planks and for how many rubles.")
(defparameter *start-money* 10)
(defparameter *start-fuel* .6)
(defparameter *mid-station-gap* 1200.0 "The stretch length above which one more gas station stands halfway.")

;;; Trade and parcels

(defparameter *parcel-base* 4 "The base pay for a parcel: this much plus *PARCEL-PER-KM* per kilometre to the addressee.")
(defparameter *parcel-per-km* 2.5)
(defparameter *market-clients* 5 "The number of clients who come to the bell at a market during one stay.")
(defparameter *client-mix* '((:sender . .35) (:seller . .25) (:buyer . .4)) "The kinds of client who come at a market and the share of each.")
(defparameter *patience* 28.0 "The number of seconds a client waits at the window.")
(defparameter *busk-every* 3.5 "The interval in seconds at which listeners toss a coin.")
(defparameter *sill-limit* 4 "The number of things that fit on the windowsill.")

;;; Things

(defparameter *crate-planks* 3 "The number of planks in a crate: placing one costs that many, taking one apart gives them back.")
(defparameter *max-items* 250 "The maximum number of things in the wagon; more are neither placed nor accepted over the network.")

;;; Stove

(defparameter *plank-burn* 75.0 "The number of seconds one plank burns.")

;;; Speeds

(defparameter *max-speed* 9.0 "The top speed of the truck with the house on it, m/s (about 32 km/h).")
(defparameter *throttle* 1.4 "The acceleration, m/s².")
(defparameter *brake* 4.5 "The braking deceleration, m/s².")
(defparameter *walk-speed* 2.2)
(defparameter *run-factor* 1.9 "The factor by which running (Shift) is faster than walking.")

;;; Things to do on the road (uyut.lisp)
(defparameter *cook-time* 28.0 "The number of seconds it takes a raw good on the hot stove to become a dish.")
(defvar *recipes* '()
  "The recipes: entries of (raw-good dish dish-name-in-the-accusative).
A dish is wanted wherever its raw good is wanted, but at twice the price. Without the cooktop the stove
bakes only blini: nothing else fits on its round lid.")
(defparameter *hob-price* 40 "The price of the cast-iron cooktop for the stove, sold at the hardware shop in Krasny Kut.")
(defparameter *blin-factor* 2 "The factor by which blini cost more than milk...")
(defparameter *blin-bonus* 3 "...and the number of rubles added on top.")
(defparameter *swatter-price* 6 "The price of the fly swatter at the gas station by the swamp.")
(defparameter *airmail-share* .3 "The share of the parcel pay that airmail takes...")
(defparameter *airmail-min* 2 "...and the minimum it takes, in rubles.")
(defparameter *bottle-price* 1 "The number of rubles paid for a bottle at the collection point.")
(defparameter *paper-price* 2 "The number of rubles paid for a bundle of waste paper.")
(defparameter *busk-live* 2 "The factor by which coins come more often for a live guitar than for a record.")

;;; Goods and towns (goods.lisp)
(defvar *goods* '()
  "The goods: entries of (kind, name in the accusative case as in 'they sell fish', average price).")
(defvar *town-trade* '()
  "The trade of each town by town number: (what is made here) (what is short here).")
(defparameter *sell-range* '(.55 .8) "The share of the average price a seller asks, as (FROM TO).")
(defparameter *buy-range* '(1.4 1.9) "The share of the average price a buyer pays, as (FROM TO).")
(defparameter *rush-town* 2 "The number of the town with the factory whistle.")
(defparameter *rush-hours* '(280.0 400.0) "The time of the shift change: seconds since dawn, as (FROM TO).")
(defparameter *rush-bonus* 1.3 "The factor by which buyers pay more during the whistle hour.")

;;; Town gimmicks (prikol.lisp)
(defparameter *range-price* 1 "The price of five shots at the shooting range.")
(defparameter *book-papers* 3 "The number of bundles of waste paper traded for books.")
(defparameter *stamp-price* 15 "The price of the town council stamp.")
(defparameter *stamp-factor* 2 "The factor by which parcels pay more with the stamp.")
(defparameter *water-price* 1 "The price of a glass of mineral water.")
(defparameter *water-time* 300.0 "The number of seconds it keeps you pepped up.")
(defparameter *water-boost* 1.35 "The factor by which you walk faster meanwhile.")
(defparameter *carpenter-deal* '(6 5) "The carpenter's deal: how many planks and for how many rubles (the gas station gives three for five).")
(defparameter *coal-burn* 3 "The factor by which coal burns longer than a plank.")

;;; Papers and the militia (bumagi.lisp)
(defparameter *check-every* 420.0 "The gap between militia checks: from this many seconds of driving up to twice as many.")
(defparameter *check-wait* 45.0 "The number of seconds the militiaman waits for the papers.")
(defparameter *cargo-free* 2 "The number of coal crates, and likewise of air cargo places, allowed without a licence: for own use.")
(defparameter *fine-cargo* 6 "The fine for carrying a forbidden good without a licence.")
(defparameter *fine-ignore* 3 "The fine for not showing the papers.")

;;; Market glut, parcels, the train (logic changes of 16.11.2026)
(defparameter *glut-step* .12 "The price shift for each recent deal in the same good in the same town.")
(defparameter *glut-floor* .45 "The share below which the buyer's price does not fall.")
(defparameter *glut-rest* 150.0 "The number of seconds it takes the market to 'forget' one deal.")
(defparameter *parcel-delivery* 6 "The pay from the recipient on delivery (the advance is half of the sender's pay).")
(defparameter *rail-per-km* 3.0 "The train fare from Uzlovaya: rubles per kilometre of road to the town...")
(defparameter *rail-min* 6 "...and the minimum fare.")

