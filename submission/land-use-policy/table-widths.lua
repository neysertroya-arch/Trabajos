-- Submission-only formatting filter.
-- Sets relative column widths for the three publication tables without changing content.

local table_index = 0

local widths = {
  {0.30, 0.07, 0.126, 0.126, 0.126, 0.126, 0.126},
  {0.22, 0.30, 0.14, 0.14, 0.10, 0.10},
  {0.46, 0.22, 0.16, 0.16}
}

function Table(tbl)
  table_index = table_index + 1
  local target = widths[table_index]
  if target ~= nil and #tbl.colspecs == #target then
    for i, w in ipairs(target) do
      tbl.colspecs[i][2] = w
    end
  end
  return tbl
end
