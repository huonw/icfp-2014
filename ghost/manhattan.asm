include "consts.asm"

const WALL_PENALTY 255
const DIR_PENALTY 100
const LAST_DIR_PENALTY 1
const LAST_PLACE_PENALTY 2
const MY_INDEX_PENALTY 1
const OTHER_GHOST_HYPERBOLIC_DIST_PENALTY 10

const WALK_ON_PILLS_BONUS 1
const PLAYER_DRIFT_BONUS 10
const PLAYER_HYPERBOLIC_DIST_BONUS 150
const TICK_BONUS 2

decl up
decl right
decl down
decl left

decl tick

decl player_x
decl player_y

decl my_x
decl my_y

decl my_vitality
decl my_direction
decl my_index

decl candidate_x
decl candidate_y
decl candidate_square_status

decl last_dir
decl num_ghosts

;;; additive factors for each direction, to be able to loop over them
decl up_x
decl up_y
decl right_x
decl right_y
decl down_x
decl down_y
decl left_x
decl left_y
decl dir_list_end

;;; last_places is stored as [x, y, x, y, ...], these numbers are the
;;; number of cells, not the number of logical pairs.
;;; WARNING, this must be the last variable declared.
const NUM_LAST_PLACES 32
const LAST_PLACE_MASK 31
decl last_place_idx
decl last_places

main:
        @up_y = 255
        @right_x = 1
        @down_y = 1
        @left_x = 255

;;; Count how many ghosts there are
        f = @num_ghosts
        ;; but only when we haven't counted before.
        jeq count-ghosts-start, f, 0
        jump count-ghosts-exit
count-ghosts-start:
        a = f
        b = 0
        int $GHOST_START_POS

        jeq count-ghosts-exit, b, 0
        f += 1
        jump count-ghosts-start
count-ghosts-exit:
        @num_ghosts = f

        @up = 128
        @right = 128
        @down = 128
        @left = 128

        int $LAMBDA_POS_1
        @player_x = a
        @player_y = b


        int $MY_INDEX
        @my_index = a
        int $GHOST_CURR_POS
        @my_x = a
        @my_y = b

        a = @my_index
        int $GHOST_INFO
        @my_vitality = a
        @my_direction = b

;;; opposite direction penalty
        ;; flip the direction, the direction variables are at fixed
        ;; locations (exactly the corresponding direction)
        b ^= 2
        a = $DIR_PENALTY
        c = b
        call apply-penalty

;;; tie breakers:
        ;; going the same direction twice is boring
        a = $LAST_DIR_PENALTY
        c = @last_dir
        call apply-penalty

        ;; each bot is slightly different
        a = @my_index
        [a] -= $MY_INDEX_PENALTY

        ;; "random"
        a = @tick
        a &= 3
        [a] += $TICK_BONUS

;;; consider each possible move in turn
        g = &@up_x
        c = &@up
        ;; c and g should not be touched.
each-possibility-start:
        a = @my_x
        a += [g]
        g += 1
        b = @my_y
        b += [g]
        g += 1

        @candidate_x = a
        @candidate_y = b

;;; Check if this square is a wall
        int $SQUARE_STATUS
        @candidate_square_status = a
        jgt rate-wall-is-not-wall, a, $WALL
        a = $WALL_PENALTY
        call apply-penalty
        ;; no point wasting energy on this
        jump each-possibility-continue
rate-wall-is-not-wall:
        jeq not-a-pill, @candidate_square_status, $EMPTY
        jgt not-a-pill, @candidate_square_status, $FRUIT
        a = $WALK_ON_PILLS_BONUS
        call apply-bonus
not-a-pill:


;;; Check if we've been here recently
        e = &@last_places
        f = g
        f += $NUM_LAST_PLACES
visitations-start:
        a = [e]
        e += 1
        ;; last-x == current x
        jeq visitations-check-y, a, @candidate_x
        jump visitations-next
visitations-check-y:
        a = [e]
        ;; last-y == current y
        jeq visitations-penalty, a, @candidate_y
        jump visitations-next
visitations-penalty:
        ;; it's in the list!
        a = $LAST_PLACE_PENALTY
        call apply-penalty
visitations-next:
        e += 1
        jlt visitations-start, e, f

;;; Check if this square would be close.
        a = @player_x
        b = @candidate_x
        call abs-difference
        d = a

        a = @player_y
        b = @candidate_y
        call abs-difference
        d += a
        ;; only apply the bonus/penalty when the distance is closish
        jgt dist-end, d, 10
        d *= d
        d += 1

        a = $PLAYER_HYPERBOLIC_DIST_BONUS
        a /= d
        jeq dist-apply-penalty, @my_vitality, $G_FRIGHT
        call-ret-to apply-bonus dist-end
dist-apply-penalty:
        call apply-penalty
dist-end:

;;; try to avoid going to close to other ghosts, spreading out is
;;; good.
;        f = 0
;        jump other-ghosts-end
;other-ghosts-start:
;        a = f
;        jeq other-ghosts-continue, a, @my_index
;
;        int $GHOST_CURR_POS
;        e = b
;        b = @candidate_x
;        call abs-difference
;        d = a
;        a = e
;        b = @candidate_y
;        call abs-difference
;        d += a
;        ;; too far to bother
;        jgt other-ghosts-continue, d, 10
;        d *= d
;        d += 1
;        a = $OTHER_GHOST_HYPERBOLIC_DIST_PENALTY
;        a /= d
;        call apply-penalty
;
;other-ghosts-continue:
;        f += 1
;        jlt other-ghosts-start, f, @num_ghosts
;other-ghosts-end:
;
        ;; general loop infrastructure:
each-possibility-continue:
        c += 1
        jlt each-possibility-start, c, 4

;;; Try to move towards the player
        a = $PLAYER_DRIFT_BONUS
        ;; Check horizonal directions
        jgt player-to-left, @my_x, @player_x
        jeq end-player-hori, @my_x, @player_x
        c = &@right
        call-ret-to apply-bonus end-player-hori
player-to-left:
        c = &@left
        call apply-bonus
end-player-hori:

        a = $PLAYER_DRIFT_BONUS
        ;; Check vertical
        jgt player-above, @my_y, @player_y
        jeq end-player-vert, @my_y, @player_y
        c = &@down
        call-ret-to apply-bonus end-player-vert
player-above:
        c = &@up
        call apply-bonus
end-player-vert:

        ;; Find the highest rated direction.
        b = 1
        a = 0
max-start:
        jgt max-continue, [a], [b]
        a = b
max-continue:
        b += 1
        jlt max-start, b, 4

        ;; save our current move into the last-seen list
        @last_dir = a
        c = &@last_places
        c += @last_place_idx
        [c] = @my_x
        c += 1
        [c] = @my_y
        @last_place_idx += 2
        @last_place_idx &= $LAST_PLACE_MASK
        int $TELL_DIR
        inc @tick

;        a = @up
;        b = @right
;        c = @down
;        d = @left
;        debug
        hlt

;;; a = points to add, c = address to adjust
apply-bonus:
        ;;  don't bother updating h, since we don't need to
        [h] = [c]
        [c] += a
        a = [h]
        jlt apply-bonus-overflow,[c],a
        return
apply-bonus-overflow:
        [c] = 255
        return

;;; a = points to remove, c = address to adjust
apply-penalty:
        [h] = [c]
        [c] -= a
        a = [h]
        jgt apply-penalty-overflow,[c],a
        return
apply-penalty-overflow:
        [c] = 0
        return

;;; a = abs(a - b)
abs-difference:
        jlt a-d-first-small, a, b
        a -= b
        jump a-d-first-end
a-d-first-small:
        b -= a
        a = b
a-d-first-end:
        return
