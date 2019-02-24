defmodule Meetup do
  @moduledoc """
  Calculate meetup dates.
  """

  @type weekday ::
          :monday
          | :tuesday
          | :wednesday
          | :thursday
          | :friday
          | :saturday
          | :sunday

  @type schedule :: :first | :second | :third | :fourth | :last | :teenth

  @doc """
  Calculate a meetup date.

  The schedule is in which week (1..4, last or "teenth") the meetup date should
  fall.
  """
  @spec meetup(pos_integer, pos_integer, weekday, schedule) :: :calendar.date()
  def meetup(year, month, weekday, schedule) do
    # 13th	Thirteenth
    # 14th	Fourteenth
    # 15th	Fifteenth
    # 16th	Sixteenth
    # 17th	Seventeenth
    # 18th	Eighteenth
    # 19th	Nineteenth

    # first day of year
    {:ok, first_day_date} = Date.from_erl({year, month, 1})
    # check leap year
    schedule = if not Date.leap_year?(first_day_date) and month == 2 and schedule == :last, do: :fourth, else: schedule

    ref_day_of_month =
      case schedule do
        :first  -> 1
        :second -> 8
        :teenth -> 13
        :third  -> 15
        :fourth -> 22
        :last -> Date.days_in_month(first_day_date)
      end

    weekday_target =
      case weekday do
        :monday -> 1
        :tuesday -> 2
        :wednesday -> 3
        :thursday -> 4
        :friday -> 5
        :saturday -> 6
        :sunday -> 7
      end

    # gets reference date to know the day of week
    year_month_tuple = {year, month}
    ref_day_tuple = Tuple.append(year_month_tuple, ref_day_of_month)
    {:ok, ref_day_date} = Date.from_erl(ref_day_tuple)
    # gets reference day of week to shift to target
    day_of_week = Date.day_of_week(ref_day_date)

    if day_of_week == weekday_target do
      # easy hit, same day
      ref_day_tuple
    else
      day_of_month = calc_day_shift(ref_day_of_month, day_of_week, weekday_target, schedule == :last)
      Tuple.append(year_month_tuple, day_of_month)
    end
  end

  def calc_day_shift(ref_day_of_month, day_of_week, weekday_target, false) do
    if day_of_week < weekday_target do
      ref_day_of_month + (weekday_target - day_of_week)
    else
      ref_day_of_month + (7 - (day_of_week-weekday_target))
    end
  end

  def calc_day_shift(ref_day_of_month, day_of_week, weekday_target, true) do
    if weekday_target < day_of_week do
      ref_day_of_month - (day_of_week - weekday_target)
    else
      ref_day_of_month - (7 - (weekday_target-day_of_week))
    end
  end
end
