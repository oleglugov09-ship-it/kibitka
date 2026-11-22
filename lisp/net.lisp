;;;; Network play: one driver (the host) and guests. The host computes everything itself and ten times a second
;;;; broadcasts path coordinate, speed, time, weather, tank, money and who stands where; guests send where
;;;; they stand and what they moved. The guest's world is built from the same seed (towns, bus stops and
;;;; buildings are computed from the seed and the path coordinate, see RS-FOR); roadside trees are each player's own.
;;;;
;;;; On your own network the link is ENet directly between players (ENetConnection, built into Godot):
;;;; delivery is reliable and ordered, connects and disconnects arrive as events; the driver's address
;;;; is needed (local network or VPN), port 4410/UDP.
;;;; Messages are lists of numbers, strings and keywords printed with PRIN1; everything else
;;;; is dropped on input (MSG-SAFE-P).
;;;;
;;;; Over the internet, when players are behind NAT, play goes by room code: the driver and the guests each
;;;; connect outwards to the room server (relay/, Cloudflare Worker, WebSocket), and it forwards the text.
;;;; The server address is *RELAY-URL* (or KIOSK_RELAY); until it is set, play is by address only.
;;;;
;;;; The role is chosen in the lobby (lobby.lisp, F2). For testing there are environment variables:
;;;;   KIOSK_HOST=1 godot --path project
;;;;   KIOSK_JOIN=192.168.1.5 KIOSK_NAME=Vasya godot --path project
;;;;
;;;; Until the driver has left the depot, a guest appears in the wagon at the depot. Once it has left,
;;;; the guest waits at the nearest bus stop ahead and boards when the wagon stops there.

(in-package #:kiosk-trip)

(defparameter *net-port* 4410)
(defparameter *net-rate* .1 "The interval in seconds between state broadcasts.")
(defvar *net-role* nil "The network role: :host, :guest, or NIL when playing alone.")
(defvar *net-name* "Водитель")
(defvar *net-server* nil "The host's ENetConnection, listening on the port.")
(defvar *net-conn* nil "The guest's ENetConnection.")
(defparameter *net-max-peers* 8)
(defparameter *net-version*
  #.(let ((h 7))
      ;; a fingerprint of all game sources at build time: desktop and browser builds made from
      ;; the same code get the same one, even though they were built at different times
      ;; the mods are part of the rules now (recipes, prices, what grows): they go into the fingerprint too
      (dolist (file (sort (append (directory (merge-pathnames "*.lisp" (or *compile-file-truename* *load-truename*)))
                                  (directory (merge-pathnames "../project/mods/*/*.lisp" (or *compile-file-truename* *load-truename*))))
                          #'string< :key #'namestring))
        (with-open-file (in file :external-format :utf-8)
          (loop for c = (read-char in nil) while c
                do (setf h (mod (+ (* h 31) (char-code c)) 2147483629)))))
      (string-downcase (format nil "~36r" h)))
  "Версия сборки — отпечаток исходников. Играть вместе можно только на одной и той же:
иначе мир и сообщения расходятся.")
(defparameter *relay-url* "wss://kibitka-relay.liltechdude-dev.workers.dev"
  "The room server (relay/, deployed with `npx wrangler deploy`); NIL means no play by room code.")
(defparameter *relay-rate* .16 "The send interval through the room server, which is longer: on the free plan messages are counted.")
(defvar *net-ws* nil "The WebSocketPeer to the room server: for the driver its room, for a guest the link to the driver.")
(defvar *net-room* nil "The room code.")
(defvar *ws-open* nil "True once the socket to the room server has opened.")
(defvar *net-retry* nil "The time (by *TIME*) to try again, or NIL: the driver reopens the room, a guest comes back.")
(defvar *net-retries* 0 "The number of attempts made in a row so far.")
(defparameter *net-retry-max* 40 "The number of 4-second attempts after which we give up and wait for F2.")
(defvar *host-key* (string-downcase (format nil "~36r" (+ (expt 36 9) (cl:random (expt 36 9)))))
  "The random key of this copy of the game: the driver presents it to the room server.")
(defvar *net-pinged* 0.0 "The time we last confirmed to the lobby list that the room is alive.")
(defvar *net-seated* '() "The host's list of names of those who have already ridden in the wagon.
Someone who returns after a dropped link is inside again, not at a bus stop.")
(defvar *net-refused* nil "The driver's version, kept by a guest when the driver did not let us in.")
(defvar *net-peers* '()
  "The host's list of guests, each a plist.
:peer is the ENet connection or NIL, :key its number, :rid the seat in the room (a guest through the
server), :id the number in the game, :ok true when the version matched, :name :x :z :yaw :inside who
and where, :placed true once they sent where they stand, :wait the path coordinate of the bus stop
where they wait, :ver which arrangement of things they know, :mine the arrangement number after
their last edit, :node the figure at the bus stop, :told true once the driver has been warned.")
(defvar *net-peer* nil "The guest's link to the host.")
(defvar *net-id* 0 "Our number at the host.")
(defvar *net-next-id* 0)
(defvar *net-clock* 0.0)
(defvar *net-ver* 0 "The number of the arrangement of things.")
(defvar *net-specs* nil "The host's arrangement that *NET-VER* corresponds to.")
(defvar *net-others* '() "The other players in the game, each a list (id name x z yaw inside).")
(defvar *net-avatars* '() "The cats of the other players, each (id :group node :cat cat :head head :x :z :yaw :y :hop).")
(defvar *net-host* nil "The guest's last state from the host (a plist).")
(defvar *net-seen* 0.0 "The time the host last responded, kept by a guest.")
(defvar *net-lost* nil)
(defvar *net-joined* nil "True for a guest once the first state has been received and the world is built.")
(defvar *net-wait* nil "The guest's path coordinate of the bus stop where we wait for the wagon, or NIL when we are in it.")
(defvar *net-sig* nil "The guest's record of the arrangement, the table and the planks as the host knows them.")
(defvar *net-hold* 0.0 "The time until which a guest ignores someone else's arrangement (we have just edited it ourselves).")
(defvar *wait-stood* 0.0 "The number of seconds the wagon has stood away from our bus stop, for a guest waiting there.")
(defvar *took* nil "The guest's record of the thing just picked up; it goes to the driver together with the edit.")
(defvar *net-fed* 0 "The number of planks a guest has fed into the stove since the last send.")
(defvar *rejoining* nil "Bound to T while a guest is returning by itself to the same room after a dropped link.")
(defvar *net-planks-delta* 0 "The guest's change in the number of planks since the last send (feeding the stove counts as minus).")
(defvar *edit-rejected* nil "True for a guest when the driver said it did not accept our edit of the things.")
(defvar *net-pending* nil "The guest's (number arrangement) from the host that is not applied yet.")
(defvar *net-resend* 0 "The guest's marker that there is an edit to send to the host.")
(defvar *departed* nil "True once the host has left the depot.")

(defun net-guest-p () (eq *net-role* :guest))



;;; Messages

(defun net-send (peer msg)
  (object-call peer "put_var" (with-standard-io-syntax (prin1-to-string msg)) nil))

(defun msg-safe-p (x)
  "Return true if message X holds only lists, numbers, strings, keywords, T and NIL."
  (or (null x) (eq x t) (realp x) (stringp x) (keywordp x)
      (and (consp x) (loop for rest = x then (cdr rest)
                           while (consp rest)
                           always (msg-safe-p (car rest))
                           finally (return (null rest))))))

(defun number-token-p (tok)
  "Return true if TOK is written as a number.
The form is [sign] digits [. digits] [exponent: e d s f l, sign, digits].
READ takes nothing else for a number and would intern a symbol."
  (let ((i 0) (n (length tok)))
    (flet ((digits () (let ((start i)) (loop while (and (< i n) (digit-char-p (char tok i))) do (incf i)) (> i start))))
      (and (<= n 30)
           (progn (when (and (< i n) (find (char tok i) "+-")) (incf i)) t)
           (let ((whole (digits))
                 (frac (and (< i n) (char= (char tok i) #\.) (progn (incf i) (digits)))))
             (or whole frac))
           (or (= i n)
               (and (find (char tok i) "edsfl")
                    (progn (incf i) (when (and (< i n) (find (char tok i) "+-")) (incf i)) t)
                    (digits)
                    (= i n)))))))

(defun text-safe-p (text)
  "Check TEXT before reading it: only numbers, strings, keywords, T and NIL, with shallow nesting.
Otherwise READ would intern foreign symbols or fall into deep recursion before we get to look."
  (let ((depth 0) (i 0) (n (length text)))
    (flet ((token-ok-p (tok)
             (or (string= tok "T") (string= tok "NIL")
                 (and (char= (char tok 0) #\:) (<= 2 (length tok) 24)                 ; a keyword: letters and hyphen
                      (every (lambda (c) (or (alpha-char-p c) (char= c #\-))) (subseq tok 1)))
                 (number-token-p tok))))
      (loop while (< i n)
            do (let ((c (char text i)))
                 (cond ((char= c #\() (when (> (incf depth) 6) (return-from text-safe-p nil)) (incf i))
                       ((char= c #\)) (decf depth) (incf i))
                       ((char= c #\Space) (incf i))
                       ((char= c #\") (incf i)
                        (loop while (and (< i n) (char/= (char text i) #\"))
                              do (when (char= (char text i) #\\) (return-from text-safe-p nil)) (incf i))
                        (incf i))
                       (t (let ((end (or (position-if (lambda (c) (find c " ()\"")) text :start i) n)))
                            (unless (token-ok-p (subseq text i end)) (return-from text-safe-p nil))
                            (setf i end))))))
      (zerop depth))))

(defun parse-msg (text)
  "Return the message read from TEXT, or NIL if it is not one."
  (let ((msg (and (stringp text) (< (length text) 20000) (text-safe-p text)
                  (ignore-errors (with-standard-io-syntax (let ((*read-eval* nil)) (values (read-from-string text))))))))
    (and (consp msg) (msg-safe-p msg) msg)))

(defun msg-text (msg) (with-standard-io-syntax (prin1-to-string msg)))

(defun peer-msgs (peer)
  "Return all the messages that arrived from PEER (ENet), in order."
  (loop while (> (object-call peer "get_available_packet_count") 0)
        for msg = (parse-msg (object-call peer "get_var" nil))
        when msg collect msg))

;;; The room server (WebSocket)

(defun room-code-p (text)
  (and (= (length text) 4) (every #'alpha-char-p text) (every (lambda (c) (< (char-code c) 128)) text)))

(defun new-room-code ()
  (coerce (loop repeat 4 collect (char "ABCDEFGHJKLMNPRSTUVWXYZ" (random 23))) 'string))

(defun relay-url () (or (env-text "KIOSK_RELAY") *relay-url*))

(defun url-encode (text)
  (with-output-to-string (out)
    (loop for b across (ext:string-to-octets text :external-format :utf-8)
          do (if (or (<= 48 b 57) (<= 65 b 90) (<= 97 b 122))
                 (write-char (code-char b) out)
                 (format out "%~2,'0x" b)))))

(defun ws-open (code role &optional name)
  "Return a socket to room CODE, or NIL if there is no room server.
NAME is the driver's name for the lobby list."
  (let ((base (relay-url)))
    (when base
      (let ((ws (gdext:add-reference (gdext:make-object "WebSocketPeer"))))
        (cond ((eql 0 (object-call ws "connect_to_url"
                                   (format nil "~a/ws?room=~a&role=~a&v=~a~@[&name=~a~]~@[&key=~a~]" base code role *net-version*
                                           (and name (url-encode name)) (and (equal role "host") *host-key*))))
               (setf *ws-open* nil)
               ws)
              (t (gdext:release-reference ws) nil))))))

(defun ws-state (ws) (object-call ws "get_ready_state"))   ; 0 is connecting, 1 is open, 3 is closed

(defun ws-texts (ws)
  "Poll socket WS and return the strings that arrived."
  (object-call ws "poll")
  (loop while (> (object-call ws "get_available_packet_count") 0)
        collect (or (ignore-errors (utf8-text (object-call ws "get_packet"))) "")))

(defun ws-drop ()
  (when *net-ws*
    (ignore-errors (object-call *net-ws* "close" 1000 ""))
    (gdext:release-reference *net-ws*))
  (setf *net-ws* nil *net-room* nil *ws-open* nil))

(defun peer-send (p msg)
  "Send MSG from the host to guest P, directly or through the room."
  (cond ((getf p :peer) (net-send (getf p :peer) msg))
        ((and *net-ws* (= (ws-state *net-ws*) 1))
         (object-call *net-ws* "send_text" (format nil "~d|~a" (getf p :rid) (msg-text msg))))))

(defun guest-linked-p ()
  (if *net-ws* (= (ws-state *net-ws*) 1) (and *net-peer* (= (object-call *net-peer* "get_state") 5))))

(defun guest-send (msg)
  (if *net-ws* (object-call *net-ws* "send_text" (msg-text msg)) (net-send *net-peer* msg)))

(defun net-events (conn)
  "Return the ENet events of CONN for this frame as ((type . peer) ...).
Type 1 is a connect, 2 a disconnect, 3 an incoming packet."
  (loop repeat 64
        for e = (object-call conn "service" 0)
        for type = (elt e 0)
        while (plusp type)
        collect (cons type (elt e 1))))

(defun peer-key (peer) (object-call peer "get_instance_id"))

(defun clean-name (name fallback)
  (let ((n (remove-if (lambda (c) (find c "#|\"\\")) (string-trim " " (or name "")))))
    (if (plusp (length n)) (subseq n 0 (min 16 (length n))) fallback)))



(defun my-place ()
  "Return where we are in the wagon: x, z, yaw, whether inside, and eye height above the floor."
  (list (vector3-x *pos*) (vector3-z *pos*) (float (- (mod (+ *yaw* pi) (* 2 pi)) pi) 1.0) (and (eq *view* :inside) (not *meta*) t)
        (if *seat* (fifth *seat*) *eye*)))

;;; The host

(defun enet-p ()
  "Return true if this build has ENet: the browser has none, only the room server."
  (object-call (gdext:singleton "ClassDB") "can_instantiate" "ENetConnection"))

(defun net-host (&optional (name "Водитель"))
  "Open the lobby as NAME: by address on your own network and, if there is a room server, by room code."
  (net-stop)
  (let* ((server (and (enet-p) (gdext:add-reference (gdext:make-object "ENetConnection"))))
         (direct (and server (eql 0 (object-call server "create_host_bound" "*" *net-port* *net-max-peers* 1 0 0))))
         (code (new-room-code))
         (ws (ws-open code "host" (clean-name name "Водитель"))))
    (when (and server (not direct)) (gdext:release-reference server))
    (cond ((or direct ws)
           (setf *net-server* (and direct server) *net-ws* ws *net-room* (and ws code)
                 *net-role* :host *net-name* (clean-name name "Водитель") *net-peers* '() *net-next-id* 0 *net-seated* '()
                 *net-specs* (mapcar #'car *items*) *net-ver* 1)
           (say "Лобби открыто. Ждём попутчиков")
           t)
          (t (say "Не вышло открыть лобби: порт занят, сервера комнат нет") nil))))

(defun host-state (p)
  (list :s (getf p :id) *trip-seed* *dist* *speed* *day-t* *season* *weather* *fuel* *money* *planks* *fire*
        (switches)
        ;; 'already left' applies only to newcomers: whoever rode with us and dropped off returns into the wagon.
        ;; On the dev polygon, and in a game started at the tunnel, nobody waits at a bus stop: everyone lands
        ;; straight in the wagon
        (and *departed* (not *dev*) (not (tunnel-mode-p)) (not (member (getf p :name) *net-seated* :test #'equal)))
        *net-ver*
        (cons (append (list* 0 *net-name* (my-place)) (list (my-look)))
              (loop for q in *net-peers*
                    unless (or (eq q p) (getf q :wait) (not (getf q :placed)))
                      collect (list (getf q :id) (getf q :name) (getf q :x) (getf q :z) (getf q :yaw) (getf q :inside) (getf q :eye)
                                    (getf q :look 0))))
        (client-view)
        (world-for p)))                ; everything shared in one block, see sync.lisp

(defun small-p (v &optional (limit 100)) (and (realp v) (< (- limit) v limit)))
(defun far-p (v) (and (realp v) (<= 0 v 1e7)))

(defun note-items ()
  "Give the arrangement a new number because it has changed."
  (let ((specs (mapcar #'car *items*)))
    (unless (equal specs *net-specs*) (setf *net-specs* specs) (incf *net-ver*))))

(defun host-take (p msg)
  ;; first the guest states its build version; we do not play with a different version
  (when (eq (first msg) :hello)
    (cond ((equal (second msg) *net-version*)
           (setf (getf p :ok) t (getf p :name) (clean-name (and (stringp (third msg)) (third msg)) "Попутчик")))
          (t (peer-send p (list :no *net-version*))
             (if (getf p :peer)
                 (object-call (getf p :peer) "peer_disconnect_later" 0)
                 (object-call *net-ws* "send_text" (format nil "~d|!" (getf p :rid))))   ; the room server will free the seat
             (say "Кто-то стучался с другой версией игры")))
    (return-from host-take p))
  (unless (getf p :ok) (return-from host-take p))
  (case (first msg)
    (:p (destructuring-bind (name x z yaw inside wait ver &optional (eye *eye*) (look 0)) (rest msg)
          (declare (ignore ver))
          (when (and (stringp name) (small-p x) (small-p z) (small-p yaw) (or (null wait) (far-p wait)) (small-p eye 3))
            (setf (getf p :eye) eye (getf p :look) (if (look-ok-p look) look 0))
            (let ((new (not (getf p :placed))) (was (getf p :wait)))
              (setf (getf p :name) (clean-name name "Попутчик")
                    (getf p :x) x (getf p :z) z (getf p :yaw) yaw (getf p :inside) inside
                    (getf p :wait) wait (getf p :placed) t)
              (when (and new (plusp *net-ver*)) (setf (getf p :ver) 0))
              (when new (log-event :peer-joined :name (getf p :name) :wait (and wait t) :dist (round *dist*) :relay (and (getf p :rid) t)))
              (cond ((and new wait) (say (format nil "~a ждёт на остановке впереди" (getf p :name))))
                    (new (say (format nil (if *departed* "~a снова в кибитке" "~a на складе, в кибитке") (getf p :name))))
                    ((and was (not wait)) (say (format nil "~a сел в кибитку" (getf p :name)))))
              (unless (eql was wait)
                (when (getf p :node) (node-queue-free (getf p :node)))
                (setf (getf p :node) nil (getf p :told) nil))))))
    (:a (case (second msg)                                   ; the guest's action at the window
          (:do (host-do p (cddr msg)))                        ; a shared action on behalf of the guest, see sync.lisp
          (:ran (host-ran p (third msg)))                     ; the reply to a live edit
          (:bell (ring-bell))
          (:take (destructuring-bind (z kind addr props) (cddr msg)
                   (when (and (realp z) (carry-ok-p kind addr props))               ; for coins the address is the wanted good
                     (host-deal p z kind addr props))))))
    (:e (destructuring-bind (base specs fold planks-delta took fed) (rest msg)
          (when (and (specs-ok-p specs) (integerp planks-delta) (<= -12 planks-delta 12)
                     (integerp fed) (<= 0 fed 5) (or (null took) (spec-ok-p took)))
            ;; the guest sent the things as a whole list. We take it only if, since the guest saw
            ;; our arrangement, nobody else has changed it; otherwise it would overwrite someone else's work (say,
            ;; parcels just handed over), so we send the fresh one and let the guest edit again
            (note-items)
            (let ((ours (mapcar #'car *items*)) (fresh (or (eql base *net-ver*) (eql (getf p :mine) *net-ver*)))
                  (accepted t))
              (cond ((and took (not (member took ours :test #'equal)))
                     (setf accepted nil)
                     ;; the guest 'took' a thing we no longer have: another player or a customer carried it off earlier;
                     ;; otherwise two players would end up holding it
                     (peer-send p (list :x :drop))
                     (setf (getf p :ver) -1))
                    ((equal specs ours))
                    (fresh (set-items specs) (save-interior)
                           (note-items)
                           (setf (getf p :mine) *net-ver*))
                    (t (setf accepted nil)
                       (peer-send p (list :x))                 ; 'edit not accepted': the guest rechecks its hands against ours
                       (setf (getf p :ver) -1)))
              ;; a rejected edit touches nothing: the guest will get our state and edit again
              (when accepted
                (set-switches fold :guarded t)
                ;; planks arrive as a difference, not a count: a stale count from the guest would overwrite the ones
                ;; the driver has just bought; how many of them went into the stove is counted separately
                (setf *planks* (max 0 (min 99 (+ *planks* planks-delta)))
                      *fire* (min 300.0 (+ *fire* (* fed *plank-burn*))))))))))
  p)

(defun host-waiting (p)
  "Handle guest P waiting at a bus stop: put a figure there and warn the driver."
  (let* ((d (getf p :wait)) (gap (ahead-of d)))
    (when (and (not (getf p :node)) (< -20 gap *ahead*))
      (setf (getf p :node) (apply #'add-person (stop-spot d -.5))))
    (when (and (not (getf p :told)) (< 0 gap 80) (> *speed* 1.0))
      (setf (getf p :told) t)
      (say (format nil "На остановке ждёт ~a" (getf p :name)))))
  p)

(defun drop-peer (p)
  (remhash (getf p :id) *world-sent*)
  (when (getf p :node) (node-queue-free (getf p :node)))
  (when (getf p :name)
    (log-event :peer-left :name (getf p :name) :dist (round *dist*))
    (say (format nil "~a отключился" (getf p :name))))
  (when (getf p :peer) (gdext:release-reference (getf p :peer)))
  (setf *net-peers* (remove p *net-peers*)))

(defun new-peer (peer rid)
  "Return a new guest record: PEER is the direct ENet connection, RID the seat in the room.
All fields are present at once: later we edit them in place."
  (first (push (list :peer (and peer (gdext:add-reference peer)) :key (and peer (peer-key peer)) :rid rid
                     :id (incf *net-next-id*) :ok nil :name nil :x 0.0 :z 0.0 :yaw 0.0 :eye 1.55 :inside nil :wait nil
                     :ver 0 :mine nil :placed nil :node nil :told nil :look 0)
               *net-peers*)))

(defun host-room ()
  "Process the room on the server: who came in, who left, what was sent."
  (dolist (text (ws-texts *net-ws*))
    (let* ((bar (position #\| text))
           (rid (and bar (ignore-errors (parse-integer text :end bar))))
           (body (and bar (subseq text (1+ bar)))))
      (when rid
        (let ((p (find rid *net-peers* :key (lambda (q) (getf q :rid)))))
          (cond ((string= body "+") (when p (drop-peer p)) (new-peer nil rid))   ; another guest is in this seat now
                ((string= body "-") (when p (drop-peer p)))
                (p (let ((m (parse-msg body))) (when m (noting :host-take (host-take p m))))))))))
  (case (ws-state *net-ws*)
    (1 (unless *ws-open* (setf *ws-open* t) (say (format nil "Комната открыта. Код: ~a" *net-room*)))
       ;; once a minute we confirm to the lobby list that we are alive
       (when (> (- *time* *net-pinged*) 60) (setf *net-pinged* *time*) (object-call *net-ws* "send_text" "~")))
    (3 ;; the room server dropped the link (it was updated, the internet blinked): the guests fell off, and we
       ;; reopen the room with the same code ourselves; the guests come back by themselves too (STEP-GUEST)
       (dolist (p (remove nil *net-peers* :key (lambda (q) (getf q :rid)))) (drop-peer p))
       (let ((code (if (eql (object-call *net-ws* "get_close_code") 4002) (new-room-code) *net-room*)))   ; 4002: code taken
         (ws-drop)
         (cond ((< (incf *net-retries*) *net-retry-max*)
                (setf *net-room* code *net-retry* (+ *time* 5.0))
                (when (= *net-retries* 1) (say "Связь с сервером комнат пропала. Открываю комнату заново…")))
               (t (setf *net-retry* nil)
                  (say "Сервер комнат не отвечает. Игра осталась по адресу в своей сети · F2 — лобби")))))))

(defun host-reopen ()
  "Try to reopen the driver's room with the same code after it closed against our will."
  (when (and *net-retry* (> *time* *net-retry*) (null *net-ws*) *net-room*)
    (let ((code *net-room*))
      (setf *net-ws* (ws-open code "host" *net-name*) *net-room* code
            *net-retry* (and (null *net-ws*) (< (incf *net-retries*) *net-retry-max*) (+ *time* 5.0))))))

(defun step-host (tick)
  (when *net-ws* (host-room))
  (host-reopen)
  (when (and *net-ws* *ws-open*) (setf *net-retries* 0 *net-retry* nil))
  (dolist (e (and *net-server* (net-events *net-server*)))
    (destructuring-bind (type . peer) e
      (let ((p (find (peer-key peer) *net-peers* :key (lambda (q) (getf q :key)))))
        (case type
          (1 (unless p (new-peer peer nil)))
          (2 (when p (drop-peer p)))
          (3 (when p (dolist (m (peer-msgs peer)) (noting :host-take (host-take p m)))))))))
  (when (> *dist* 12) (setf *departed* t))
  (dolist (p *net-peers*)
    (cond ((not (getf p :placed)))
          ((getf p :wait) (host-waiting p))
          (t (pushnew (getf p :name) *net-seated* :test #'equal))))
  (when tick
    (note-items)
    (dolist (p *net-peers*)
      (when (getf p :name)
        (peer-send p (host-state p))
        (unless (eql (getf p :ver) *net-ver*)
          (peer-send p (list :i *net-ver* *net-specs*))
          (setf (getf p :ver) *net-ver*))))                 ; delivery is reliable, so we do not send twice
    (setf *net-others* (loop for p in *net-peers*
                             when (and (getf p :placed) (not (getf p :wait)))
                               collect (list (getf p :id) (getf p :name) (getf p :x) (getf p :z) (getf p :yaw) (getf p :inside) (getf p :eye)
                                             (getf p :look 0))))))

;;; The guest

(defun net-join (where &optional (name "Попутчик"))
  "Connect to the driver as NAME.
WHERE is a room code (four letters) or an address on your own network."
  (unless (or *rejoining* (net-guest-p)) (stow-carry))     ; we do not carry our own things into someone else's game
  (when *meta* (setf *meta* nil) (apply-meta))
  (net-stop)
  (flet ((joined (text)
           (setf *net-role* :guest *net-name* (clean-name name "Попутчик") *net-joined* nil *net-wait* nil
                 *net-host* nil *net-seen* *time* *net-lost* nil *net-ver* 0 *speed* 0.0)
           (say text) t))
    (if (room-code-p where)
        (let ((ws (ws-open (string-upcase where) "guest")))
          (cond (ws (setf *net-ws* ws *net-room* (string-upcase where))
                    (joined (format nil "Ищем комнату ~a…" *net-room*)))
                (t (say "Игра по коду пока не настроена. Нужен адрес водителя") nil)))
        (let ((conn (and (enet-p) (gdext:add-reference (gdext:make-object "ENetConnection")))))
          (cond ((null conn) (say "В браузере — только по коду комнаты") nil) ((and (eql 0 (object-call conn "create_host" 1 1 0 0))
                      (let ((peer (object-call conn "connect_to_host" where *net-port* 1 0)))
                        (when peer (setf *net-peer* (gdext:add-reference peer)))))
                 (setf *net-conn* conn)
                 (joined (format nil "Стучимся к ~a…" where)))
                (t (object-call conn "destroy") (gdext:release-reference conn)
                   (say "Адрес не подходит") nil))))))

(defun jump-dist (d)
  "Move to path coordinate D in one go.
Roadside things get built in the window around the new place by themselves; the land under it is
built at once if it is not there yet."
  (setf *dist* (float d 1.0))
  (note-side-roads t)
  (settle-land)
  (step-props))

;;; The shared switches of the wagon: whoever flips one, it changes for everyone. They travel as one list in
;;; the same message field that used to hold the table alone.

(defun switches ()
  "Return the switches as they are now for us: (shelf lights window shared door ladder)."
  (list (and *fold-down* t) (and *lights-on* t) (and *window-open* t) (common-state)   ; fourth: the common things (DEFCOMMON, delo.lisp)
        (and *door-open* t) (and *ladder-down* t)))

(defun set-switches (list &key guarded)
  "Set the switches to the ones received in LIST.
With GUARDED (on the driver's side) the window cannot be closed while a customer is at it or things
stand on the windowsill, the same as by hand."
  (when (and (listp list) (= (length list) 6))
    (destructuring-bind (fold lights window spare door ladder) list
      (accept-common spare guarded)
      (when (and guarded *window-open* (not window) (window-busy-p)) (setf window t))
      (unless (eq (and fold t) (and *fold-down* t)) (toggle-fold))
      (unless (eq (and lights t) (and *lights-on* t)) (setf *lights-on* (and lights t)) (apply-lights))
      (setf *window-open* (and window t)
            *door-open* (and door t) *ladder-down* (and ladder t)))))

(defun guest-sig () (list (mapcar #'car *items*) (switches) *planks*))

(defun guest-first (h)
  "Handle the first state H from the host: build the same world and decide where to appear."
  (setf *net-joined* t)
  (reset-commons)                                         ; somebody else's wagon: its common things are taken as they are
  (log-event :guest-joined :departed (getf h :departed) :dist (round (getf h :dist)) :name *net-name*)
  (start-trip :seed (getf h :seed))
  (setf *seat* nil *pos* (v3 .2 0 1.2))
  (cond ((getf h :departed)
         (jump-dist (getf h :dist))
         (setf *net-wait* :pick *view* :wait)
         (say "Кибитка уже в пути. Ждёшь её на остановке впереди"))
        (t (unless (load-place) (setf *view* :inside))     ; rejoined: back to where we sat
           (when (eq *view* :outside) (setf *view* :inside))
           (say (if (> (getf h :dist) 12)
                    "Ты снова в кибитке"
                    "Ты на складе, в кибитке. Ждём, пока водитель тронется")))))

(defun guest-pick-stop (h)
  "Pick a bus stop ahead of the wagon from host state H and stand at it."
  (let ((stop (first (sort (remove-if (lambda (s) (< (first s) (+ (getf h :dist) 45))) (copy-list *stops*)) #'< :key #'first))))
    (cond (stop (setf *net-wait* (first stop) *yaw* (float pi 1.0) *pitch* 0.0)
                (jump-dist (first stop)))
          (t (jump-dist (+ *dist* 150.0))))))          ; no bus stops in sight yet: look further along the road

(defun place-truck (h)
  "Place the wagon where the host has it by state H, while we stand at the bus stop."
  (let* ((hd (getf h :dist)) (s *dist*) (c (+ (road-frame s) *yaw-now*))
         (a (road-pos hd *lane-now*)) (b (road-pos s *lane-now*)) (dx (- (first a) (first b))) (dz (- (third a) (third b))))
    ;; the offset on the map, in the axes of our stand: its x is sideways to the right (cos c, sin c), z is back (-sin c, cos c)
    (node-3d-set-position *cart* (v3 (+ (* dx (cos c)) (* dz (sin c))) (- (road-y hd) (road-y s))
                                     (+ (* dx -1 (sin c)) (* dz (cos c)))))
    (node-3d-set-rotation *cart* (v3 0 (- c (road-frame hd) *yaw-now*) 0))))

(defun guest-aboard ()
  (setf *net-wait* nil *view* :inside *seat* nil *pos* (v3 0 0 2.0) *yaw* 0.0)
  (node-3d-set-position *cart* (v3 0 0 0))
  (node-3d-set-rotation *cart* (v3 0 0 0))
  (if (< (getf *net-host* :dist) (- *dist* 5))
      (relocate (getf *net-host* :dist))
      (jump-dist (getf *net-host* :dist)))
  (log-event :guest-boarded :dist (round *dist*))
  (say "Кибитка встала. Ты внутри"))

(defun guest-take (msg)
  (case (first msg)
    (:no (setf *net-refused* (if (stringp (second msg)) (second msg) "другая") *net-lost* t)
         (say "У водителя другая версия игры. Нужна одинаковая сборка"))
    (:g (destructuring-bind (changed kind addr words props) (rest msg) (guest-dealt changed kind addr words props)))
    (:x (if (eq (second msg) :drop)
            (progn (setf *carry* nil *carry-from* nil) (show-carry) (say "Эту вещь уже забрали"))
            (setf *edit-rejected* t)))
    (:r (guest-done (rest msg)))                              ; the driver did our action
    (:play (start-play (second msg)))                         ; the driver let us into the booth game (igry.lisp)
    (:c (guest-run (second msg)))                             ; a live edit from the driver: run it and reply
    (:t (when (stringp (second msg)) (say (second msg))))     ; said to everyone
    (:s (destructuring-bind (you seed dist speed day-t season weather fuel money planks fire fold departed ver others
                             &optional client world)
            (rest msg)
          (when (listp client) (noting :guest-client (guest-client client)))
          (when (and world *net-joined*) (noting :apply-world (apply-world world)))
          (unless (and (integerp you) (integerp seed) (far-p dist) (small-p speed) (far-p day-t)
                       (member season *seasons*) (keywordp weather) (small-p fuel) (integerp money) (integerp planks)
                       (small-p fire 1000) (integerp ver) (listp others) (< (length others) 20)
                       (every (lambda (o) (and (fits o '(integer string t t t t t t)) (look-ok-p (eighth o))
                                               (small-p (third o)) (small-p (fourth o)) (small-p (fifth o)) (small-p (seventh o) 3)))
                              others))
            (return-from guest-take nil))
          (setf *net-id* you *net-seen* *time* *net-lost* nil
                *net-host* (list :seed seed :dist dist :speed speed :departed departed :ver ver)
                *net-others* (remove you others :key #'first))
          (unless *net-joined* (guest-first *net-host*))
          (setf *day-t* day-t *fuel* fuel *money* money *departed* departed)
          (unless (eq season *season*) (set-season season))
          (unless (eq weather *weather*) (set-weather weather))
          (when (> *time* *net-hold*)
            (set-switches fold)
            (setf *planks* planks)
            (when (> (abs (- fire *fire*)) 3) (setf *fire* fire))
            (setf *net-sig* (guest-sig)))))
    (:i (destructuring-bind (ver specs) (rest msg)
          (when (specs-ok-p specs) (setf *net-pending* (list ver specs)))))))      ; STEP-GUEST applies it once we stop editing

(defun guest-apply-items ()
  (when (and *net-pending* (> *time* *net-hold*))
    (destructuring-bind (ver specs) *net-pending*
      (unless (equal specs (mapcar #'car *items*)) (set-items specs))
      (when *edit-rejected* (setf *edit-rejected* nil) (reconcile-carry specs))
      (setf *net-ver* ver *net-pending* nil *net-sig* (guest-sig)))))

(defun guest-rejoin ()
  "Knock on the same room again after the guest's link dropped."
  (when (and *net-retry* (> *time* *net-retry*) *net-room*)
    (let ((room *net-room*) (name *net-name*) (tries (1+ *net-retries*)))
      (let ((*rejoining* t)) (net-join room name))   ; resets the counters, so we restore them; hands and things stay
      (setf *net-retries* tries *net-retry* nil))))

(defun note-own-edit ()
  "Remember that we, a guest, changed something (things, switches, planks) and send it to the driver.
Called BEFORE incoming messages are handled: otherwise a state arriving in the same frame would
record our edit as 'what the driver has'."
  (when *net-joined*
    (let ((sig (guest-sig)))
      (when (and *net-sig* (not (equal sig *net-sig*)))
        (setf *net-planks-delta* (+ *net-planks-delta* (- (third sig) (third *net-sig*)))
              *net-sig* sig *net-hold* (+ *time* .8) *net-resend* 1)))))

(defun step-guest (tick)
  (guest-rejoin)
  (note-own-edit)
  (when (and *net-joined* (not *net-lost*)) (setf *net-retries* 0))
  (flet ((linked () (setf *net-seen* *time*)
                   (guest-send (list :hello *net-version* *net-name*))
                   (say "Подключились. Ждём водителя"))
         (lost (why) (unless *net-lost* (setf *net-lost* t) (log-event :net-lost :why why :joined *net-joined*) (say why))))
    (cond (*net-ws*
           (dolist (text (ws-texts *net-ws*))
             (let ((m (parse-msg text))) (when m (noting :guest-take (guest-take m)))))
           (case (ws-state *net-ws*)
             (1 (unless *ws-open* (setf *ws-open* t) (linked)))
             (3 (let ((code (object-call *net-ws* "get_close_code")))
                  ;; we have already ridden together, so we try to return by ourselves: the link may have just blinked while
                  ;; the driver is reopening the room at that moment. We do not try if we were kicked for a reason.
                  (when (and (not *net-lost*) (not (member code '(4003 4005))) (or *net-joined* (plusp *net-retries*))
                             (< *net-retries* *net-retry-max*))
                    (setf *net-retry* (+ *time* 4.0)))
                  (lost (case code
                          (4003 "В комнате нет мест · F2 — лобби")
                          (4005 "У водителя другая версия игры. Обнови игру · F2 — лобби")
                          (t (cond (*net-retry* "Связь пропала. Пробую вернуться…")
                                   ((= code 4004) "Такой комнаты нет. Проверь код · F2 — лобби")
                                   ((= code 4001) "Водитель закрыл игру · F2 — лобби")
                                   (*ws-open* "Связь с водителем пропала · F2 — лобби")
                                   (t "Сервер комнат не отвечает · F2 — лобби")))))))))
          (t
           (dolist (e (net-events *net-conn*))
             (case (car e)
               (1 (linked))
               (2 (lost (if *net-joined* "Связь с водителем пропала. F2 — лобби" "Водитель не отвечает. F2 — лобби")))
               (3 (dolist (m (peer-msgs (cdr e))) (noting :guest-take (guest-take m)))))))))
  (let ((h *net-host*))
    (when (and h *net-joined*)
      (cond ((eq *net-wait* :pick) (setf *speed* 0.0) (guest-pick-stop h))
            (*net-wait*
             (setf *speed* 0.0)
             (place-truck h)
             (let ((gap (- *net-wait* (getf h :dist))))
               ;; the wagon stands and is not coming to us (the driver is trading, the autopilot target is nearer than
               ;; our bus stop): after 12 seconds we assume we walked to it ourselves
               (if (< (getf h :speed) .4) (incf *wait-stood* (/ 1.0 60)) (setf *wait-stood* 0.0))
               (cond ((or (and (< (abs gap) 9) (< (getf h :speed) .4)) (> *wait-stood* 12.0))
                      (setf *wait-stood* 0.0)
                      (guest-aboard))
                     ((< (* *dir* gap) -25) (setf *net-wait* :pick)
                      (say "Кибитка проехала мимо. Ждёшь на следующей остановке")))))
            (t (let ((diff (- (getf h :dist) *dist*)))
                 (setf *speed* (getf h :speed))
                 (cond ((< diff -60) (relocate (getf h :dist)))          ; the driver 'turned around' to a town behind
                       ((> diff 60) (jump-dist (getf h :dist)))
                       (t (incf *dist* (* .08 diff)))))))
      (guest-apply-items)))
  (when (and tick (not *net-lost*) *net-joined* (guest-linked-p))
    (destructuring-bind (x z yaw inside eye) (my-place)
      (guest-send (list :p *net-name* x z yaw inside (and (realp *net-wait*) *net-wait*) *net-ver* eye (my-look))))
    (when (> *net-resend* 0)
      (decf *net-resend*)
      (guest-send (list :e *net-ver* (mapcar #'car *items*) (switches) *net-planks-delta* *took* *net-fed*))
      (setf *net-planks-delta* 0 *took* nil *net-fed* 0))))

;;; The other players in the wagon

(defun turn-toward (from to k)
  "Return angle FROM moved towards TO by fraction K along the shorter arc."
  (let ((d (- (mod (+ (- to from) pi) (* 2 pi)) pi)))
    (+ from (* k d))))

(defun step-avatars (delta)
  "Step the cats of the other players by DELTA.
Height goes by the eyes: a cat is scaled so that its eyes are at the height of the player camera
(*EYE*); when the player sits down, the cat drops by the same amount (the bell-shaped coat sinks
into the seat, as if it sat). A position arrives several times a second: the cat does not jump to
it but walks there smoothly, hopping as it goes."
  (let ((shown (remove-if-not #'sixth *net-others*)))
    (setf *net-avatars*
          (remove-if (lambda (a) (unless (find (first a) shown :key #'first) (node-queue-free (getf (rest a) :group)) t)) *net-avatars*))
    (dolist (o shown)
      (destructuring-bind (id name x z yaw inside eye &optional (look 0)) o
        (declare (ignore inside))
        (let ((a (assoc id *net-avatars*)))
          (when (and a (/= look (getf (rest a) :look 0)))             ; new outfit or flashlight click: rebuild the cat
            (node-queue-free (getf (rest a) :group))
            (setf *net-avatars* (remove a *net-avatars*) a nil))
          (unless a
            (let* ((g (group *cart* 0 *floor* 0)) (made (make-cat g id look)) (cat (first made)) (k (/ *eye* *cat-eye-y*)))
              (node-3d-set-scale cat (v3 k k k))
              (let ((tag (sign g name 0 (+ *eye* .6) 0 :h .09 :yaw 0.0)))      ; the name: over the head, or just over a lying cat
                (godot:node-3d-set-position tag (v3 0 (if (look-lying-p look) .7 (+ *eye* .6)) 0)))
              (let ((kind (look-hold look)))                                 ; what the cat holds: in front of its chest
                (when kind
                  (let ((paws (group cat .16 .62 .34)))
                    (if (eq kind :kettle)
                        (kettle-model paws 0 0 0)
                        (node-3d-set-scale (build-item paws kind 0 (- *floor*) 0 0) (if (member kind '(:box :crate)) (v3 .45 .45 .45) (v3 1 1 1)))))))
              (case (look-torch look)                                        ; flashlight: the cat faces +z
                (1 (torch-model (second made) .2 -.08 .3 :yaw pi))          ; beam forward
                (2 (torch-model (second made) 0 -.42 .3 :yaw pi :chin t)))   ; upright under the muzzle: spooky face
              (setf a (first (push (list id :group g :cat cat :head (second made) :look look :x x :z z :yaw yaw :y 0.0 :hop 0.0)
                                   *net-avatars*)))))
          (let* ((st (rest a))
                 (k (min 1.0 (* delta 9.0)))
                 (dx (- x (getf st :x))) (dz (- z (getf st :z)))
                 (far (> (+ (* dx dx) (* dz dz)) 4.0))                       ; sat or jumped: no dragging across the wagon
                 (nx (if far x (+ (getf st :x) (* k dx)))) (nz (if far z (+ (getf st :z) (* k dz))))
                 (moving (> (sqrt (+ (* dx dx) (* dz dz))) .06))
                 (lying (look-lying-p look))                                 ; on a bunk: the eyes are just above its mattress
                 (y (if lying (- eye .12) (max -.6 (min 0.0 (- eye *eye*)))))  ; sat down: lower by the eye height difference
                 (hop (+ (getf st :hop) (* k (- (if moving 1.0 0.0) (getf st :hop))))))
            (setf (getf (rest a) :x) nx (getf (rest a) :z) nz (getf (rest a) :hop) hop
                  (getf (rest a) :yaw) (turn-toward (getf st :yaw) (if lying pi yaw) k)
                  (getf (rest a) :y) (+ (getf st :y) (* k (- y (getf st :y)))))
            ;; got out at a gas station: the cat stands on the ground, not at floor height
            (node-3d-set-position (getf st :group) (v3 nx (+ (if (outdoors-p nx nz) (ground-rise nx nz) *floor*) (getf (rest a) :y)) nz))
            (node-3d-set-rotation (getf st :group) (v3 0 (+ (getf (rest a) :yaw) pi) 0))
            ;; walking is a hop with a slight forward lean
            ;; lying: the cat is on its back along the bunk, its head where the player's eyes are
            (node-3d-set-position (getf st :cat) (if lying (v3 0 .2 *eye*) (v3 0 (* hop .09 (abs (sin (* *time* 11.0)))) 0)))
            (node-3d-set-rotation (getf st :cat) (if lying (v3 (- (/ pi 2)) 0 0) (v3 (* hop .08) 0 0)))
            ;; the head is separate from the body: it floats and sways, and lags behind the hop when walking
            (node-3d-set-position (getf st :head)
                                  (v3 0 (+ *cat-head-y* (* .012 (sin (* *time* 2.3))) (* hop .04 (abs (sin (- (* *time* 11.0) .9))))) 0))
            (node-3d-set-rotation (getf st :head) (v3 0 0 (* .05 (sin (* *time* 1.7)))))))))))

(defun my-addresses ()
  "Return this machine's addresses on the local network and VPN; they are told to fellow travellers."
  (remove-if-not (lambda (a) (and (not (find #\: a))
                                  (or (eql 0 (search "192.168." a)) (eql 0 (search "10." a)) (eql 0 (search "172." a)))))
                 (coerce (object-call (gdext:singleton "IP") "get_local_addresses") 'list)))

(defun roster ()
  "Return who is in the game, with the driver marked: for the Tab list."
  (flet ((names (list) (format nil "~{~a~^, ~}" list)))
    (case *net-role*
      (:host (names (cons (format nil "~a (водитель)" *net-name*) (mapcar #'second *net-others*))))
      (:guest (names (cons *net-name* (loop for o in *net-others*
                                            collect (if (eql (first o) 0) (format nil "~a (водитель)" (second o)) (second o))))))
      (t *net-name*))))

(defun net-hud ()
  "Return the network line for the screen.
It is usually empty: the room code and the player list are on Tab; by itself it shows only link
trouble and waiting at a bus stop."
  (let ((tab (input-is-key-pressed 4194306)))                     ; Tab
    (case *net-role*
      (:host (cond (tab (format nil "~@[Комната: ~a~%~]В кибитке: ~a~@[~%Ждут на остановке: ~d~]"
                                (and *ws-open* *net-room*) (roster)
                                (let ((n (count-if (lambda (p) (getf p :wait)) *net-peers*))) (and (plusp n) n))))
                   ((and *net-room* (not *ws-open*)) "Комната открывается…")
                   (t "")))
      (:guest (cond ((and *net-lost* *net-retry*) (format nil "Связь пропала. Пробую вернуться… (~d)" (1+ *net-retries*)))
                    (*net-refused* (format nil "Версии разные: у водителя ~a, у тебя ~a.~%Обнови игру · F2 — лобби" *net-refused* *net-version*))
                    (*net-lost* "Нет связи с водителем · F2 — лобби")
                    ((not *net-joined*) "Подключаемся…")
                    ((realp *net-wait*) (format nil "Ждёшь кибитку: до неё ~d м" (round (abs (- *net-wait* (getf *net-host* :dist))))))
                    (tab (format nil "~@[Комната: ~a~%~]В кибитке: ~a" *net-room* (roster)))
                    (t "")))
      (t ""))))

(defun net-stop ()
  "Close the network game and restore everything it moved: the view, the wagon, the counters."
  (reset-commons)
  (dolist (p *net-peers*)
    (when (getf p :node) (node-queue-free (getf p :node)))
    (when (getf p :peer)                                  ; a guest through the room server has no direct connection
      (object-call (getf p :peer) "peer_disconnect_now" 0)
      (gdext:release-reference (getf p :peer))))
  (when *net-peer*
    (when (= (object-call *net-peer* "get_state") 5) (object-call *net-peer* "peer_disconnect_now" 0))
    (gdext:release-reference *net-peer*))
  (ws-drop)
  (dolist (conn (list *net-server* *net-conn*))
    (when conn (object-call conn "destroy") (gdext:release-reference conn)))
  (dolist (a *net-avatars*) (node-queue-free (getf (rest a) :group)))
  (when *client* (client-off *client*))                   ; the customer at the window belonged to that game
  (setf *client* nil *client-seen* nil)
  ;; we were a guest and are leaving for good: hands empty, and things, money, tank and place on the road are ours again.
  ;; (When returning to the same room after a dropped link we touch nothing: the thing in hand is still ours.)
  (when (and (eq *net-role* :guest) *cart* (not *rejoining*))
    (setf *carry* nil *carry-from* nil *auto* nil *net-role* nil)
    (show-carry)
    (start-trip)
    (set-items (load-interior))
    (unless (load-game) (setf *onboard* 0))
    (load-world))                                         ; papers, finds, booths, gnomes, the wall are ours again, not the host's
  (when (eq *view* :wait)                                 ; waited at a bus stop: back into the wagon
    (setf *view* :inside *seat* nil *pos* (v3 0 0 1.2))
    (when *cart* (node-3d-set-position *cart* (v3 0 0 0)) (node-3d-set-rotation *cart* (v3 0 0 0))))
  (setf *net-server* nil *net-conn* nil *net-peer* nil *net-peers* '() *net-avatars* '() *net-others* '() *net-role* nil
        *net-wait* nil *net-sig* nil *net-lost* nil *net-refused* nil *net-pending* nil *net-resend* 0
        *net-joined* nil *net-host* nil *net-retry* nil *net-retries* 0))

(defun step-net (delta)
  (when *net-role*
    (let ((tick (>= (incf *net-clock* delta) (if *net-ws* *relay-rate* *net-rate*))))
      (when tick (setf *net-clock* 0.0))
      (if (eq *net-role* :host) (step-host tick) (step-guest tick))
      (step-avatars delta))))

(defun env-text (var)
  "Return environment variable VAR as text: ECL hands out the UTF-8 bytes one by one."
  (let ((raw (ext:getenv var)))
    (when (and raw (plusp (length raw)))
      (or (ignore-errors (utf8-text (map '(vector (unsigned-byte 8)) #'char-code raw)))
          raw))))

(defun net-from-env ()
  "Take the network role from environment variables; see the file header."
  (let ((name (env-text "KIOSK_NAME")) (join (ext:getenv "KIOSK_JOIN")))
    (cond ((and join (plusp (length join))) (net-join join (or name "Попутчик")) t)
          ((ext:getenv "KIOSK_HOST") (net-host (or name "Водитель")) t)
          ((ext:getenv "KIOSK_SOLO") t))))                      ; straight into the game, without lobby or network (testing)
