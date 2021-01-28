#!/usr/bin/env racket
#lang racket
(require 2htdp/universe
         2htdp/image)

;; Размеры игрового пространства.
(define width 1000)
(define height 800)

;; Частота появления врагов (0 - 999)
(define spawn-chance 985)

;; Расстояние для коллизии
(define target-distance 40)

;; Модуль ускорения при нажатии на кнопку "вперед".
(define key-accel 2)

;; Модуль угловой скорости при нажатии на кнопки поворота.
(define key-ω (/ pi 40))

;; Множитель для гашения скорости.
(define damp 0.6)

;; Состояние "мира".
(struct world
  (x y        ; координаты
     vx vy      ; вектор скорости
     accel      ; модуль ускорения
     α          ; угол поворота относительно Ox
     ω          ; угловая скорость
     scores     ; очки
     tgx tgy    ; координаты цели
     enemies    ; список врагов
     shots)     ; список выстрелов 
  #:transparent)

;; Исходное состояние 
(define world0 (world (/ width 2) (/ height 2) 0 0 0 0 0 0 100 100'()'()))

;; Выстрел
(struct shot
  (x y        ; координаты
   vx vy)     ; скорость  
  #:transparent)

;; Противник
(struct enemy
  (x y        ; координаты
   vx vy)     ; скорость    
  #:transparent)


;; Обработка нажатий и отпусканий кнопок.
;; press? — истина, если клавиша нажата, ложь — отпущена.
;; w — состояние игрового мира,
;; k — клавиша.
(define ((control press?) w k)
  (let ((new-a (if press? key-accel 0.0))
        (new-ω (if press? key-ω 0.0))
        (new-shots (if press? (world-shots w) (append (list(emit-shot w)) (world-shots w)) )))
    (cond
      ((key=? k "up")
       (struct-copy world w (accel new-a)))
      ((key=? k "left")
       (struct-copy world w (ω (- new-ω))))
      ((key=? k "right")
       (struct-copy world w (ω new-ω)))
      ((key=? k " ")
       (struct-copy world w (shots new-shots)))
      (else w))))

;; Создает один выстрел
(define (emit-shot w)
  ;; Захват нужных полей структуры world.
  (match-define
    (struct* world ((x x) (y y) (vx vx) (vy vy) (α α)))
    w)
  (let* ((pα (+ α 3.14))                ; угол направления
         (pv 85)                        ; значение скорости
         (pvx (- (* (cos pα) pv)))      ; вектор скорости
         (pvy (- (* (sin pα) pv))))
    (shot (+ x pvx)
          (+ y pvy)
          (+ pvx vx)         
          (+ pvy vy))))

;; Обновляет состояние выстрела p
(define (update-shot p)
  (match-define (shot x y vx vy) p)
  (shot (+ x vx)
        (+ y vy)
        vx
        vy))

;; Обновляет состояния выстрелов, удаляя находящиеся за сценой
(define (update-shots shots enemies)
  (map update-shot
       (filter (λ (p) (shoot-cheker (shot-x p) (shot-y p) enemies))shots)))

(define (shoot-cheker x y enemies)
  (and (< x width)
       (> x 0)
       (< y height)
       (> y 0)))

;; Создает одного противника
(define (emit-enemy w)
  ;; Захват нужных полей структуры world.
  (match-define
    (struct* world ((x x) (y y) (vx vx) (vy vy) (α α)))
    w)
  ;; Пояляются от нижнего края сцены, двигаясь с постоянной скоростью вверх
  (let ((pv -4)) ; значение скорости
    (enemy (random 0 ( - width 38))
           height
           0         
           pv)))

;; Обновляет состояние врага p.
(define (update-enemy p)
  (match-define (enemy x y vx vy) p)
  (enemy (+ x vx)
         (+ y vy)
         vx
         vy))

;; Обновляет состояния врагов
(define (update-enemies w)
  (map update-enemy
       (filter (λ (p) (outside? (enemy-x p) (enemy-y p) (world-x w) (world-y w) (world-shots w) (world-enemies w)))
               (world-enemies w))))

;; Проверка на достижение конца, коллизию с игроком и выстрелами
(define (outside? ex ey x y shots enemies)
  (and (>= ey 40)
       (>= (+ (sqr (- ex x))
              (sqr (- ey y)))
           (sqr target-distance))
       (>= (+ (sqr (- ex x))
              (sqr (- ey y)))
           (sqr target-distance))
       (null? (killed ex ey shots))))

(define (killed ex ey shots)
  (filter (λ (s) (<= (+ (sqr (- ex (shot-x s))) (sqr (- ey (shot-y s)))) (sqr target-distance))) shots))

;; Возвращает новое состояние мира по прошествии единицы времени.
(define (tick w)
  (match-define (world x y vx vy accel α ω scores tgx tgy enemies shots) w)
  (let* (;; Поворот (применяем угловую скорость).
         (α (+ α ω))
         ;; Вектор ускорения.
         (ax (* (cos α) accel))
         (ay (* (sin α) accel))
         ;; Вектор скорости.
         (vx (+ vx ax))
         (vy (+ vy ay))
         ;; Перемещение с проверкой на границы
         (x (if (edge? w) x (round (+ x vx))))
         (y (if (edge? w) y (round (+ y vy))))
         (scores (if (taken? w) (+ 1 scores) scores ))
         (tgx (if (taken? w) (random 40 width) tgx))
         (tgy (if (taken? w) (random 40 height) tgy))
         ;; Обновляем врагов
         (enemies (update-enemies w))
         ;; Добавляем новых врагов
         (enemies (if (<= (random 1 1000) spawn-chance)
                      enemies
                      (cons (emit-enemy w) enemies)))
         ;; Обновляем выстрелы
         (shots (update-shots shots enemies)))
    (world x y (* vx damp) (* vy damp) accel α ω scores tgx tgy enemies shots)))

;; Проверка на границы
(define (edge? w)
  (or (and (>= (world-x w)(- width 20)) (> (world-vx w) 0))
      (and (>= (world-y w)(- height 60)) (> (world-vy w) 0))
      (and (<= (world-x w) 35) (< (world-vx w) 0))
      (and (<= (world-y w) 35) (< (world-vy w) 0))))

;;Сбор флага
(define (taken? w)
  (<= (+ (sqr (- (world-tgx w)(world-x w)))
        (sqr (- (world-tgy w)(world-y w))))
     (sqr target-distance)))

;; Изображение границы
(define border-pic
  (scale/xy 3 1 (bitmap "images/border.png")))

;; Изображение игрока
(define rocket-pic
  (scale/xy 0.5 0.5 (bitmap "images/tank_player.png")))

;; Изображение цели
(define target-pic
  (scale/xy 0.2 0.2 (bitmap "images/target.png")))

;; Изображение выстрела
(define shot-pic
  (scale/xy 0.3 0.3 (bitmap "images/fireball.png")))

;; Добавляет противника e к картинке scene
(define (place-enemy e scene)
  (place-image enemy-pic
               (enemy-x e)
               (enemy-y e)
               scene))

;; Добавляет выстрел s к картинке scene
(define (place-shot s scene)
  (place-image shot-pic
               (shot-x s)
               (shot-y s)
               scene))

;; Изображение земли
(define ground
  (scale/xy 1 1 (bitmap "images/ground.png")))

;;Изображение противника
(define enemy-pic
  (scale/xy 0.5 0.5 (bitmap "images/tank_enemy.png")))

;; Отрисовка сцены
(define (draw w)
  (let* ((s ground)
         (s (place-image border-pic 0 20 s))
         (s (place-image target-pic
                         (world-tgx w) (world-tgy w)
                         s))
         (s (foldl place-enemy s (world-enemies w)))
         (s (foldl place-shot s (world-shots w)))
         (s (place-image (rotate (radians->degrees
                                  (- (* pi 3/2)
                                     (world-α w)))
                                 rocket-pic)
                         (world-x w) (world-y w)
                         s))
         (s (place-image (text (string-append "Scores: " (number->string (world-scores w))) 30 "black" )(- width 80) 45 s)))
    s))

;; Победа в игре при сборе 10 флагов
(define (done? w)
  (or (> (world-scores w) 9)
      (reach? w)))

;; Поражение в игре при достижении противником верхнего края
(define (reach? w)
  (> (length(filter (λ (p) (<= (enemy-y p) 40)) (world-enemies w))) 0 ))

;; Отрисовка финальной сцены
(define (last-picture w)
  (let* ((s ground)
         (s (if (< (world-scores w) 10)
               (place-image(text (string-append "Game over,\nYou get: " (number->string (world-scores w))) 34 'black) (/ width 2) (/ height 3) s)
               (place-image(text (string-append "You win,\nYou get: " (number->string (world-scores w))) 34 'black) (/ width 2) (/ height 3) s))))s))

;; Инициализация игры
(define (start)
  (big-bang world0
    (on-tick tick)
    (on-key (control #t))
    (on-release (control #f))
    (to-draw draw)
    (stop-when done? last-picture)))

;; Запускать игру, если файл запустили как скрипт или программу.
(module+ main
  (start))
