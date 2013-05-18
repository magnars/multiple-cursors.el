ENV["WATCHR"] = "1"
system 'clear'

def run(cmd)
  `#{cmd}`
end

def run_all_tests
  system('clear')
  result = run "./run-tests.sh"
  puts result
end

def run_test(file)
  system('clear')
  result = run "./run-tests.sh #{file} --verbose"
  puts result
end

run_all_tests
watch('.*.feature') { |file| run_test file }
watch('.*.el') { run_all_tests }

# Ctrl-\
Signal.trap 'QUIT' do
  puts " --- Running all tests ---\n\n"
  run_all_tests
end

@interrupted = false

# Ctrl-C
Signal.trap 'INT' do
  if @interrupted then
    @wants_to_quit = true
    abort("\n")
  else
    puts "Interrupt a second time to quit"
    @interrupted = true
    Kernel.sleep 1.5
    # raise Interrupt, nil # let the run loop catch it
    run_all_tests
    @interrupted = false
  end
end
