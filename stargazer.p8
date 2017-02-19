pico-8 cartridge // http://www.pico-8.com
version 8
__lua__
--ê         stargazer         ê
--ê                           ê
-- STARGAZER --
-- Relax and draw constellations on your own personal sky.

-- INSTRUCTIONS --
-- Move with arrow keys. Hold C to move faster.
-- Select stars with X. Connect them to form constellations.
-- Hold X when not connecting stars to finalise a constellation. Select a name and confirm with X, or cancel with C.
-- Delete connections with X while hovering the line.
-- To clear your constellations and generate a new sky, hold Down and C at the bottom of the sky.

sky_size = 256

state = 0 -- 0: stargen, 1: star select, 2: star name

function line(x1, y1, x2, y2, col, draw_len, gap_len)
  draw_len = draw_len or 1
  gap_len = gap_len or 0

  local flipx, flipy = false, false
  if y2 < y1 then
    y1,y2 = y2,y1
    flipy = true
  end

  if x2 < x1 then 
    x1,x2 = x2,x1
    flipx = true
  end

  local dx = x2 - x1
  local dy = y2 - y1

  local swap = false
  if dx < dy then
    x1,y1 = y1,x1
    x2,y2 = y2,x2
    dx,dy = dy,dx
    flipx,flipy = flipy,flipx --we're swapping the axes on everything, so we should include the flips
    swap = true
  end

  local px_count = 0

  local err = 0
  local deltaerr = dy / dx
  local y = y1
  for x=x1,x2 do
    if px_count < draw_len then
      local px,py = x,y
      if (flipx) px = x1 - px + x2
      if (flipy) py = y1 - py + y2
      if (swap) px,py = py,px
      pset(px,py,col)
    end
    px_count = (px_count + 1) % (draw_len + gap_len)
    
    err += deltaerr
    if err >= 0.5 then
      y += 1
      err -= 1
    end
  end
end

local function angle_in_range(x, l, s)
  return (flr(x - (l + s)) + 1) <= (x - l)
end

function circ_custom(x0, y0, r, col, theta, delta) 
  theta = theta or 0
  delta  = delta or 1
  local x = r
  local y = 0
  local err = 0

  local function set_px(x, y)
    if angle_in_range(atan2(x, y), theta, delta) then
      pset(x0 + x, y0 + y, col)
      return true
    else
      return false
    end
  end

  local continue = true
  while continue and x >= y do
    continue = false

    continue = set_px(x, y) or continue
    continue = set_px(y, x) or continue
    continue = set_px(-y, x) or continue
    continue = set_px(-x, y) or continue
    continue = set_px(-x, -y) or continue
    continue = set_px(-y, -x) or continue
    continue = set_px(y, -x) or continue
    continue = set_px(x, -y) or continue

    y += 1
    err += 1 + 2*y
    if 2*(err-x) + 1 > 0 then
      x -= 1
      err += 1 - 2*x
    end
  end
end

  -- deletes the elements in t at idx_start and idx_start + 1, and shifts all following elements down to fill the hole
function del_idx_pair(t, idx_start)
  local i = idx_start + 2
  while i <= #t do
    t[i - 2] = t[i]
    i += 1
  end

    -- remove the last 2 elements
  t[#t] = nil
  t[#t] = nil
end

  --checks if x1,y1 is within dist distance from x2,y2
function dist_check(x1,y1,x2,y2,dist)
    --if outisde bounding box, not in distance
  if x1 < x2 - dist or x1 > x2 + dist or y1 < y2 - dist or y1 > y2 + dist then return false end
  local xdiff = x2 - x1
  local ydiff = y2 - y1
  if xdiff * xdiff + ydiff * ydiff < dist * dist then return true end
  return false
end

stars={}

---- star_gen start ----
do
  local max_candidates = 10
  local stars_limit = 128 --max number of stars, estimated by sky_size^2/((mindist - pertubation/2)^2 * pi). important for knowing bit number for storing star indices

  local twinkling_base = 150
  local twinkling_rnd = 120

  local pertubation
  local mindist
  local active

  local function make_star(x,y)
    local star = {}
    star.x = x
    star.y = y
    star.base_col = 4 + flr(rnd(3))
    if(star.base_col == 4) star.base_col = 1
    set_twinkle(star)
    star.timer -= flr(rnd(twinkling_base))
    star.idx = #stars + 1 --assumes it will be added to the end of stars
    return star
  end

  function star_gen_init()
      --do rnd things in init so we can call seed before this in _init
    mindist = 19 + rnd(2)
    pertubation = 8 + rnd(4) --chosen as smallest possible mindist - 1 / sqrt(2), so that no 2 stars can be next to each other even if mindist is minimal, and they get maximum pertubation
    active = {}

    stars[1] = make_star(sky_size/2, sky_size/2)

    add(active, 1)
  end

  local function rect_check(x,y,x1,y1,x2,y2)
    if x<x1 or x > x2 or y < y1 or y > y2 then return false end
    return true
  end

  function set_twinkle(star)
    star.timer = twinkling_base + flr(rnd(twinkling_rnd))
    star.col = star.base_col + flr(rnd(2))
    if(star.col == 2) star.col = 13
  end

  function star_gen_update()
    local active_idx = active[flr(rnd(#active))+1]
    local active_star = stars[active_idx]
    local i=1
    while i<=max_candidates do
      local r = rnd(mindist) + mindist
      local angle = rnd(1)

      local new_x = flr(cos(angle) * r) + active_star.x
      local new_y = flr(sin(angle) * r) + active_star.y

      if rect_check(new_x, new_y, 0, 0, sky_size-1, sky_size-1) then
        local valid = true
        for j=1,#stars do
          local other_star = stars[j]

          if dist_check(new_x, new_y, other_star.x, other_star.y, mindist) then 
            valid = false
            break
          end
        end
        if valid then
          new_x += flr(rnd(2*pertubation) - pertubation)
          new_y += flr(rnd(2*pertubation) - pertubation)
          if rect_check(new_x, new_y, 0, 0, sky_size-1, sky_size-1) then --pertubation might move the star off-world
            local new_star = make_star(new_x, new_y)
            add(stars, new_star)
            add(active, #stars)
            break
          end
        end
      end
      i += 1
    end
    if i == max_candidates + 1 then --all candidates failed, remove this point form the active list
      del(active, active_idx)
    end

    if #active == 0 or #stars == stars_limit then
      data_load_constellations()
      star_select_update_max_stars()
      state = 1
    end
  end
end
---- star_gen end ----

abc = 'abcdefghijklmnopqrstuvwxyz123456789 ' --character set for constellation naming, needed by both data and star select section

---- data api start ----
do
  data_head = 0

  function write_bytes(data)
    if #data == 0 then do return end end

    for i=1,#data do
      data[i] = shr(data[i], 16) --move to the lowest 8 bits so after this we only have to do left shifts
    end

    local to_write = #data
    if data_head % 4 ~= 0 then --handle partial byte beginning
      local partial_num = dget(flr(data_head / 4))

      local align_offset = data_head - flr(data_head/4) * 4 --offset from the last 4byte alignment

      local add_count = min(4-align_offset, to_write) --we'll fill up the partial byte, but we have to stop if there's not enough data to do that
      for i=1,add_count do
        partial_num = bor(partial_num, shl(data[i], (4 - align_offset - i) * 8))
      end
      dset(flr(data_head/4), partial_num)
      to_write -= add_count
      data_head += add_count
    end

    if to_write >= 4 then --there's at least one full 4byte number left
      local block_count = flr(to_write / 4) --number of 4byte blocks we have
      local base_idx = (#data - to_write)
      for i=1,block_count do
        local num = shl(data[base_idx + 1], 24)
        num = bor(num, shl(data[base_idx + 2], 16))
        num = bor(num, shl(data[base_idx + 3], 8))
        num = bor(num, shl(data[base_idx + 4]))
        dset(flr(data_head / 4), num)
        data_head += 4
        base_idx += 4
        to_write -= 4
      end
    end

    if to_write > 0 then --handle remaining non-aligning bytes
      local base_idx = #data - to_write
      local partial_num = 0
      for i=1,to_write do
        local data_byte = data[base_idx + i]
        partial_num = bor(partial_num, shl(data_byte, (4 - i) * 8))
      end
      dset(flr(data_head / 4), partial_num)
      data_head += to_write
    end
  end

  function write_num(num)
    local bytes = {}
    bytes[1] = shr(band(num, 0xff00), 8)
    bytes[2] = band(num, 0x00ff)
    bytes[3] = shl(band(num, 0x0.ff00), 8)
    bytes[4] = shl(band(num, 0x0.00ff), 16)
    write_bytes(bytes)
  end

  --[[
  function read_num(idx)
    if idx % 4 == 0 then return dget(flr(idx / 4))
    else
      local first_num = dget(flr(idx / 4))
      local second_num = dget(flr(idx / 4) + 1)

      local align_offset = idx - flr(idx / 4) * 4

      second_num = shr(second_num, (4 - align_offset) * 8)

      local mask = shl(0x0.0001, align_offset * 8) - 0x0.0001
      second_num = band(second_num, mask)
      
      first_num = shl(first_num, align_offset * 8)
      mask = bnot(mask)
      first_num = band(first_num, mask)

      return bor(first_num, second_num)
    end
  end
  --]]

  function read_num(idx)
    if idx % 4 == 0 then return dget(flr(idx/4)) end

    local bytes = read_bytes(idx, 4)

    local num = shl(bytes[1], 8)
    num = bor(num, bytes[2])
    num = bor(num, shr(bytes[3], 8))
    num = bor(num, shr(bytes[4], 16))

    return num
  end

  function read_bytes(idx, len)
    local bytes = {}

    local num = dget(flr(idx / 4)) --load first num in case idx is not on a boundry
    for i=idx, idx+len - 1 do
      if i % 4 == 0 then num = dget(i/4) end --load new number when we cross a 4byte boundry

      local align_offset = i - flr(i/4) * 4

      local byte = num

      if align_offset == 0 then byte = shr(byte, 8)
      elseif align_offset == 2 then byte = shl(byte, 8)
      elseif align_offset == 3 then byte = shl(byte, 16) end

      byte = band(byte, 0xff)

      add(bytes, byte)
    end
    return bytes
  end
end
---- data api end ----

---- serialization start ----
do
  local name_reverse = {}
  function data_init()
    cartdata('keymaster_stargazer')

    for i=1,#abc do
      name_reverse[sub(abc,i,i)] = i
    end
  end

  function data_init_seed()
    local seed = read_num(0) --on first start this is 0
    seed = band(seed, 0xffff.0000) --seed is in the first 16 bits (non-fractional bits)

    if seed == 0 then
      seed = rnd(32767.99)
      seed = seed * sgn(rnd(1) - 0.5)
      seed = flr(seed)

        -- split up the seed into bytes so we can write only the first 2 bytes
      local bytes = {}
      add(bytes, band(shr(seed, 8), 0xff)) -- higher bits
      add(bytes, band(seed, 0xff)) --lower bits

      write_bytes(bytes)
    end

    srand(seed)
  end

  function data_load_constellations()
    local new_head = 4 --start behind seed

    local const_descr = read_num(new_head)

    while const_descr ~= 0 do
      local const = {}
      local star_count = band(shr(const_descr, 8), 0xff)
      local letters = {}
      letters[1] = band(shr(const_descr, 2), 0x3f)
      letters[2] = band(shl(const_descr, 4), 0x3f)
      letters[3] = band(shl(const_descr, 10), 0x3f)
      letters[4] = band(shl(const_descr, 16), 0x3f)

      const.name = ''
      for letter in all(letters) do const.name = const.name .. sub(abc, letter, letter) end

      new_head += 4

      local star_bytes = read_bytes(new_head, star_count)

      for byte in all(star_bytes) do
        
        local star_idx = band(byte, 0x7f)
        local star = stars[star_idx]
        add(const, star)
        if band(byte, 0x80) ~= 0 then add(const, star) end
      end

      set_mid_point(const)
      add_constellation(const)

      new_head += star_count

      const_descr = read_num(new_head)
    end

    data_head = new_head
  end

  function data_write_constellation(const)
    local data = {}

    -- do stars first since we need star count in our description
    local i = 1
    while i<=#const do
      local byte = const[i].idx
      if i ~= #const then
        if const[i] == const[i+1] then
          byte = bor(byte, 0x80) --bit 7 indicates if this star repeats
          i += 1 --skip the next star
        end
      end
      add(data, byte)
      i += 1
    end

    local name_nums = {}
    for i=1,4 do --since data packing doesn't work for other count, hardcoding name len of 4
      add(name_nums, name_reverse[sub(const.name, i, i)])
    end

    local descr = shl(#data,8)
    descr = bor(descr, shl(name_nums[1], 2))
    descr = bor(descr, shr(name_nums[2], 4))
    descr = bor(descr, shr(name_nums[3], 10))
    descr = bor(descr, shr(name_nums[4], 16))

    write_num(descr)
    write_bytes(data)
  end

    --gives the cost to serialize the stars of a given constellation - ignores the 4 bytes for the description!
  function data_constellation_stars_cost(const)
    local cost = 0
    local i = 1
    while i <= #const do
      if i ~= #const then
        if const[i] == const[i+1] then i += 1 end
      end
      cost += 1
      i += 1
    end

    return cost
  end
end
---- serialization end ----

---- star select start ----
do
  local cur_constellation = {}
  local hovered_star = nil
  local constellations = {}
  local commit_count = -1
  local hovered_connection = nil
  local max_stars_remaining = 256 -- will be updated after 

  function star_select_update_max_stars()
    max_stars_remaining = 256 - data_head - 4
  end

  function set_mid_point(const)
    local mid_point = {x=0, y=0}

    for star in all(const) do
      mid_point.x += star.x
      mid_point.y += star.y
    end

    mid_point.x /= #const
    mid_point.y /= #const

    const.mid_point = mid_point
  end

  function star_select_update()
    csr_gfx = 0
    
    hovered_star = nil

    for i=1,#stars do
      local star = stars[i]
      star.timer -= 1
      if dist_check(csr.x + cam.x, csr.y + cam.y, star.x, star.y, 3) then
        hovered_star = star
        csr_gfx = 1
        if btnp(5) then
          if star ~= last_star then
            if max_stars_remaining - data_constellation_stars_cost(cur_constellation) > 0 then
              if last_star ~= nil then add(cur_constellation, star) end --complete the last to our previous last star if we aren't starting a new sequence
              add(cur_constellation, star) --start the next line segment
              last_star = star
            end
          end
        end
        break
      end
    end

    hovered_connection = nil
    if hovered_star == nil and #cur_constellation ~= 0 then
      if last_star == nil then
        -- look for hovered star connection
        for i=1,#cur_constellation / 2 do
          local l_start = cur_constellation[i*2-1]
          local l_end = cur_constellation[i*2]
          local dir = {x=0,y=0}
          dir.x = l_end.x - l_start.x
          dir.y = l_end.y - l_start.y

          local csr_rel = {x=csr.x,y=csr.y}
          csr_rel.x += cam.x
          csr_rel.y += cam.y

          csr_rel.x -= l_start.x
          csr_rel.y -= l_start.y

          local along_line = (csr_rel.x * dir.x + csr_rel.y * dir.y) / (dir.x * dir.x + dir.y * dir.y)

          if along_line >= 0 and along_line <= 1 then
            local away_sqr = csr_rel.x * csr_rel.x + csr_rel.y * csr_rel.y - along_line^2 * (dir.x * dir.x + dir.y * dir.y)
            if away_sqr < 3^2 then
              hovered_connection = {
                start_idx = (i*2 - 1),
                stars = {l_start, l_end}
              }
              break
            end
          end
        end

        if hovered_connection ~= nil then
          if btnp(5) then
            del_idx_pair(cur_constellation, hovered_connection.start_idx)
            commit_count = -1 --delete should not start commit, button will have to be re-pressed to do so
          end
        else
          if btn(5) then
            if commit_count ~= -1 then commit_count += 1 end
          else
            commit_count = 0
          end

          if commit_count == 20 then
            commit_count = -1
            set_mid_point(cur_constellation)
            state = 2
          end
        end
      else --last_star is nil
        if btnp(5) then
          last_star = nil
          cur_constellation[#cur_constellation] = nil --delete the last star since it's the start of our line segment we won't complete
          commit_count = -1
        end
      end
    end
  end

  local function draw_constellation(const, col, draw_len, gap_len)
    for i=1,#const/2 do --#const/2 since we record to/from star pairs
      local from = const[i*2 - 1]
      local to = const[i*2]
      line(from.x, from.y, to.x, to.y, col, draw_len, gap_len)
    end
  end

  function star_select_draw_constellations()

    for const in all(constellations) do
      draw_constellation(const, 5, 4, 2)
    end

    draw_constellation(cur_constellation, 5)

    if hovered_connection ~= nil then line(hovered_connection.stars[1].x, hovered_connection.stars[1].y, hovered_connection.stars[2].x, hovered_connection.stars[2].y, 9) end

    if last_star != nil then 
      if hovered_star ~= nil then line(last_star.x, last_star.y, hovered_star.x, hovered_star.y, 5)
      else line(last_star.x, last_star.y, csr.x + cam.x, csr.y + cam.y, 5) end
    end
  end

  function star_select_draw_space_indicator()
    if #cur_constellation ~= 0 then
      local stars_cost = data_constellation_stars_cost(cur_constellation)
      local disp_string = stars_cost .. '/' .. max_stars_remaining

      local text_col = 13
      if max_stars_remaining - stars_cost == 0 then text_col = 8 end

      print(disp_string, 64 - (#disp_string * 4 - 1) / 2, 120, text_col)
    end
  end

  function star_select_draw_commit()
    if commit_count > 0 then
      circ_custom(csr.x, csr.y, 8, 3, 0, commit_count/20)
    end
  end

  local letters = {1,1,1,1}
  local highlighted = 1

  function add_constellation(const)
    add(constellations, const)
  end

  function star_select_update_naming()
    if btnp(4) then
      state = 1
      return
    end
    if not btn(5) then
      commit_count = 0
    end
    if btnp(5) and commit_count ~= -1 then
      cur_constellation.name = ''
      for i=1,#letters do
        cur_constellation.name = cur_constellation.name .. sub(abc, letters[i], letters[i])
        letters[i] = 1
      end
      add_constellation(cur_constellation)
      data_write_constellation(cur_constellation)
      star_select_update_max_stars()
      cur_constellation = {}
      highlighted = 1
      state = 1
      return
    end

    if btnp(0) then highlighted -= 1 end
    if btnp(1) then highlighted += 1 end

    highlighted = ((highlighted - 1) % #letters) + 1

    if btnp(2) then letters[highlighted] += 1 end
    if btnp(3) then letters[highlighted] -= 1 end
    letters[highlighted] = ((letters[highlighted] - 1) % #abc) + 1
  end

  function star_select_draw_name_select()
    local name_x = 64 - (#letters*4 - 1) / 2
    
    for i=1,#letters do
      local letter_y = 10
      if i == highlighted then
        pal(7, 11)
        letter_y -= 1
      else pal() end

      local letter_x = name_x + (i-1)*4
      
      spr(2, letter_x, letter_y - 3)
      spr(2, letter_x, letter_y, 1, 1, false, true)

      print(sub(abc, letters[i], letters[i]), letter_x, letter_y, 7)
    end
    pal()
  end

  function star_select_draw_names()
    for const in all(constellations) do
      print(const.name, const.mid_point.x - (#letters*4 - 1) / 2, const.mid_point.y - 2.5, 12)
    end
  end
end
---- star select end ----

---- restart commit start ----
do
  local restart_counter = 0
  local restart_choice = false
  local buttons_released = false

  function restart_update()
    if not restart_choice then
      if csr.y + cam.y == 255 and btn(3) and btn(4) and not (btn(0) or btn(1) or btn(2)) then
        restart_counter += 1
        if restart_counter == 42 then restart_choice = true end
      else
        restart_counter = 0
      end
    end
      
    if restart_choice then
      if not btn(4) and not btn(5) then
        buttons_released = true
      end

      if buttons_released then
        if btnp(5) then
          
            --clear all saved data, including seed
          data_head = 0
          for i=0,63 do
            write_num(0)
          end

          run()
        elseif btnp(4) then
          restart_choice = false
          buttons_released = false
        end
      end
    end
  end

  function restart_draw()
    if restart_counter >= 10 then
      clip(0,128 - (restart_counter - 10) * 4, 128, (restart_counter - 10) * 4)

      rectfill(0, 127 - (restart_counter - 10) * 4,127,127,1)
      print('a new sky?', 64 - (10*4 - 1) / 2, 64 - 4, 12)
      print('[x] yes | [0] no', 64 - (16*4 - 1) / 2, 64 + 3, 13)

      clip()
    end
  end
end
---- restart commit end ----

csr={
  x=64,
  y=64
}

csr_gfx = 0

cam={
  x=sky_size / 2 - 64,
  y=sky_size / 2 - 64
}

fade_lut = {7,6,5,1,0}
received_input = 0
step_fade = 1

function _init()
  data_init()
  data_init_seed()
  star_gen_init()
  music(1, 1500)
end

function _update()
  if state == 0 then star_gen_update()
  elseif state == 1 then
    local csr_step = 1
    if(btn(4)) then csr_step = 4 end
    if(btn(0)) then csr.x -= csr_step end
    if(btn(1)) then csr.x += csr_step end
    if(btn(2)) then csr.y -= csr_step end
    if(btn(3)) then csr.y += csr_step end

    if received_input == 0 then
      if btn(0) or btn(1) or btn(2) or btn(3) then received_input = 1 end
    end

    csr.x = mid(0, csr.x, 127)
    csr.y = mid(0, csr.y, 127)

    if(csr.x < 4) then cam.x -= csr_step end
    if(csr.x >= 124) then cam.x += csr_step end
    if(csr.y < 4) then cam.y -= csr_step end
    if(csr.y >= 124) then cam.y += csr_step end

    cam.x = mid(cam.x, 0, sky_size - 128)
    cam.y = mid(cam.y, 0, sky_size - 128)

    star_select_update()

    restart_update()
  elseif state == 2 then
    star_select_update_naming()
  end
end

function _draw()
  camera()
  rectfill(0, 0, 127, 127, 0)

  camera(cam.x, cam.y)

  if state == 1 or state == 2 then star_select_draw_constellations() end

  camera()

  for star in all(stars) do
    if star.timer == 0 then
      set_twinkle(star)
    end
    if state == 0 then pset(star.x * 128/sky_size, star.y * 128/sky_size, star.col)
    else pset(star.x - cam.x, star.y - cam.y, star.col) end
  end

  camera(cam.x, cam.y)
  if state == 1 or state == 2 then star_select_draw_names() end

  camera()
  spr(csr_gfx, csr.x-3, csr.y-3)

  if state == 0 then 
    print('filling the sky...', 29, 30, 7)

  elseif state == 1 then 
    star_select_draw_space_indicator()
    star_select_draw_commit()
  end

  local function print_w_bg(text, midx, y, text_col, bg_col)
    local width = #text*4 - 1
    local left = midx - width / 2
    local right = midx + width / 2
    rectfill(left, y, right - 1, y + 4, bg_col)
    print(text, left, y, text_col)
  end

  
  if received_input < 4 then

    pal(7, fade_lut[1 + received_input])
    pal(6, fade_lut[2 + received_input])
    pal(5, fade_lut[3 + received_input])
    if received_input > 0 and step_fade == 0 then
      received_input += 1
    end

    step_fade = (step_fade + 1) % 4
    
    spr(16, 0, 0, 16, 3)
    print_w_bg('game by', 64, 128 - 39, 6, 0)
    print_w_bg('@keymaster', 64, 128 - 32, 7, 0)

    print_w_bg('music by', 64, 128 - 22, 6, 0)
    print_w_bg('by @pizzamakesgames', 64, 128 - 15, 7, 0)

    pal()
  end

  if state == 2 then star_select_draw_name_select() end

  if state == 1 then restart_draw() end
end
__gfx__
00000000007070000700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00707000070007007770000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
07000700700000700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
07000700700000700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00707000070007000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000007070000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000007777700000000000000000000007000000000000700000000000000000000000000000000000000000000000000000
00000000000000000000770000000007770000000000000000777000000770000000000007700000000777700000007700000000000000000000000000000000
00000000000000000077007000077770000000000000000077007000007000000000000007700000000000077700000707777777700070777770000000000000
00000000000000000070000777770000000000070000770700007000070000000000000007700000000000000077000070000000000070000070000000000000
00000000000000000700007700070000000000770000707000007000700000000000000070700000000000000007000070000000000070000070000000000000
00000000000000000700000000070000000007700000707000770007000000000000000070700000000000000770000070000000000700000700000000000000
00000000000000000700000000070000000070700000770000700007000000000000000070700000000000007000000070000000000700000700000000000000
00000000000000000700000000070000000070700000700007000007000000777700000700700000000000770000000070077770000700077000000000000000
05050606070700000070000000700000000700700000700070000070000077000700000700700007700077000000000777700000000700070000707060605050
00000000000000000070000000700007777777777707700700000070000000000700777777777777000700000000000070000000007007700000000000000000
00000000000000000007000000700000070000700007000700000070000000077700007000070000007000000000000070000000070007000000000000000000
00000000000000000000700007000000700000700007000770000070000000077700070000070000070000000000000700000000070000700000000000000000
00000000000000000000700007000077000000700007000070000077000000770700070000070000070000000000007700000000070000077000000000000000
00000000000777000000700000000070000000700007000007000000777777007000700000077000007000000000777077777700700000000770000000000000
00000000000007777777000000000000000000700007000000700000000000007007700000007700007777777777770000000007000000000007700000000000
00000000000000000000000000000000000007700007000000077000000000070007000000000700000000000000000000000007000000000000000000000000
00000000000000000000000000000000000000000000000000000770000000070000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__label__
00000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010000000000000000000000
0d000000000000000000000000000000000000010000000000000000000000000000000000007000000000000000000000000000000000000000000000000000
00000000000000060000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000070000000000000000000000000000000000000000060000000000000000000000000000000000
00000000000000000000000000000000007777700000000000000000000007000000000000700000000000000000000000000000000000000000000000000000
00000000000000000000770000000007770000000000000000777000000770000010000007700000000777700000007700000000000000000000000000000000
00000000000000000077007000077770000000000000000077007000007000000000000007700000000000077700000707777777700070777770000000000000
0000000000000000007000077777000000000007000077070000700007000000000000000770000000000d000077000070000000000070000070000000000000
00000000000000000700007700070000000000770000707000007000700000000000000070700000000000000007000070000000000070000070000006000000
00000010000000000700000000070000000007700000707000770007000000000000000070700000000000000770000070000000000700000700000000000000
00000000000000000700000000070000000070700000770000700007000000000000000070700000000000007000000070000000000700000700000000000000
00000000000000000700000000670000000070700000700007000007000000777700000700700000000000770000000070077770000700077000000000000000
05050606070700000070000000700000000700700000700070000070000077000700000700700007700077000000000777700000000700070000707060605050
00000000000000000070000000700007777777777707700700000070000000000700777777777777000700000000000070000000007007700000000000000000
0000000000000000000700000070000007000070000700070000007000000007770000700007000000700000000000007000000007d007000000000000000000
00000000000000000000700007000000700050700007000770000070000000077700070000070000070000000000000700000000070000700000000000000000
00000000000000000000700007000077000000700007000070000077000000770700070000070000070000000000007700000000070000077000000000000000
00000000000777000000700000000070000000700007000007000600777777007000700000077000007000000000777077777700700000000770000010000000
00000000000007777777000000000000000000700007000000700000000000007007700000007700007777777777770000000007000000000007700000000000
00000000000000000000000000000000000007700007000000077000000000070007000000000700000000000000000000000007000000000000000000000000
00000000000000000000000000000000000000000000000000000770000000070000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000600000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000d00000000000000000000000000000000000000060000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000007000000000000000000000000000000000
00000000000000000000000000000777077707000700077707700077000607770707077700000077070707070000000000000000000000000000000000000000
00000000000000000000000000000700007007000700007007070700000000700707070000000700070707070000000000000000000000000000000000000000
00000000000000000000000000000770007007000700057007070700000000700777077000000777077007770000000000000000000000000000000000000000
00006000000000000000000000000700007007000700007007070707000000700707070000000007070700070000000000000000000000000000000000000000
00000000000000000000000000000700077707770777077707070777000000700707077700000770070707770070007000700000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000050000000000000000000000000000060000000000000060000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000d0000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000600000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000070000000000000070000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000d000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000005000000000000000000000000000000000000000000000000000000070000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000500000000000006000000000
00000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000d00000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000700000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000600000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000600000000000000000000000000000000000000000000000000d00000000000100
00000000000000000000000005000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000006000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000d0000000000000000000000000000000000
00000000000000000000000000000000000d00000000000000000000000000070700000000000000000600000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000700070000000000000000000000000000000000000050000000000000000000000
00000000000000000000000000000000000000000000000000000000000000005000000000000000000000000000000000000000000000000000000000000000
00000600000000000000000000000000000000000000000000000000000000700070000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000070700000000d00000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000d000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000700000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000600000000000000001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000500000000000000000000
00000000000000000000000000000000000000000000000000000050000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000070000000050000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000700000000100000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000500000000000000000000000000000000000000000000000000000000000000000000000
00000000000000600000000000000000000000000005000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000006000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00d00000000000000000d00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000000000010
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000600000000000000000000000000
00000000000000000000000000000000000000000000000000066066606660666000006660606000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000600060606660600000006060606000060000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000600066606060660000006600666000000000000000001000000000000000000000000000000000
00000000000000000000000000000000000000000000000000606060606060600000006060006000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000666060606060666000006660666000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000500000000
00000000000000000000000000000000000000000000000060000000000000000000500000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000100000070070707770707077707770077077707770777000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000707070707000707077707070700007007000707000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000707077007700777070707770777007007700770000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000700070707000007070707070007007007000707000000000000000000000000000000000000000000000
00000000000000000000000000006000000000000000077070707770777070707070770007007770707000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000d000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000500000000000000000000000000000000000000000000000000000000000
00000000000000060000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00006000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000066606060066066600660000066606060000000000000000000100000000000000000000000000000
00000000000000000000000000000000000010000000000066606060600006006000000060606060000000000000000000000000000000000000060000000000
00000000000000000000000000000000000000000000000060606060666006006000000066006660000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000060606060006006006000000060600060005000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000060600660660066600660000066606660000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000777070700000070077707770777077707770777077707070777007700770777077707770077000000000000000000000000000
00000000000000000000000000707070700000707070700700007000707070777070707070700070007000707077707000700000000000000000000000000000
00000000000000060000000000770077700000707077700700070007007770707077707700770077707000777070707700777000000000000000000000000000
00000000000000000000000000707000700000700070000700700070007070707070707070700000707070707070707000007000000006000000000000000000
00000000000000000000000000777077700000077070007770777077707070707070707070777077007770707070707770770000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000006000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000005000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000007000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000600000000000000000000000000000000000000000000000000000000000000000000000000000000000d00000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000600000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000700000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000

__gff__
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__map__
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__sfx__
001400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
003200001d000000000000000000000000000000000000001f0501f0301f010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001c00000614006040062400614006040062400614006040121460604006240121460604006240121460604003140030400324003140030400324003140030400f14603040032400f14603040032400f14603040
011c0000147361b72627716127161e7162a7260f7362071627600086000660000000000000000000000000001f300003110032100331003410033100321003111930000000193000000000000000000000000000
001c0000123111231114321143211433114331143311433114321143211431114311143111431114321143210d3310d3310d3310d3310d3310d3310d3210d3210f3110f3110f3310f3310f3210f3210f3110f311
000e00001425214342142321432214212143121625216332162521634216232163221622216322162221632218153164001630016400163001640018153164001630016400163001640018153164001630016400
010e00001626216352162421633216222163121225212332122521234212232123221222212322122221232218053123001805312300122001230018053123001220012300122001230018053123001220012300
001c0000000050000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000126050f605126050f6050f2200f1200f2260f220
001c00000614006140060400614006140060400614006140065400614006040061400614006040061400614003140031400304003140031400304003140031400354003140030400314003140030400f14603140
001c00000d1500d1500d0500d1500d1500d0500d1500d1500d5500d1500d0500d1500d1500d0500d1500d1500a1500a1500a0500a1500a1500a0500a1500a7500a5500a1500a0500a1500a1500a0500a1500a150
001c000011153116050d635111430c605186250f133276020c1230c1230c6550c1130c1130c6550c1130c1130e6050060500605006050e605006052860226602000000000018605006051860500605186050c000
000e00000e1530000000000286051c6351c0000e1432460200000000000c63518605186250000000000000001815300000000000000000000000000c153000000000000000000000000000153000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__music__
00 49084344
00 09080744
01 05020904
00 06020904
00 05020904
00 06020904
00 0a020904
02 080b0904
02 080b0904
02 4b484944
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344

