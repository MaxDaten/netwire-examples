# A sample Guardfile
# More info at https://github.com/guard/guard#readme


guard :shell do
  watch /.*\.l?hs$/ do |m|
    `runghc #{m[0]}`
  end
end

# vim:ft=ruby
