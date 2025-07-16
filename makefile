# Target to build the mmu program
mmu: mmu.cpp
	# Use g++ to compile with debugging info
	g++ -g mmu.cpp -o mmu

# Clean target to remove the executable and backup files
clean:
	# Remove the mmu executable and backup files
	rm -f mmu *~
