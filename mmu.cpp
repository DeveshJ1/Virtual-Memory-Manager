//include the necessary libraries
#include <iostream>
#include <string>
#include <unistd.h>
#include <fstream>
#include <sstream>
#include <algorithm>
#include <vector>
#include <queue>
namespace mmu{
    //global variable representing number of frames from the arguments
    unsigned int num_frames=0;
    //global variable indicating current process id
    unsigned int curr_pid=0;
    //global variable representing instruction count needed for output
    unsigned long instruction_count=0;
    //global variable representing # of context switches needed for output
    unsigned long context_switch=0;
    //global variable representing # of process exits needed for output
    unsigned long process_exits=0;
    //global variable representing total cost needed for output
    unsigned long long total_cost=0;
    //global variable representing max # of virtual apges which we set as 64
    //as mentioned in lab instructions
    unsigned int  max_vpage=64;
    //vector to whole the random integers
    std::vector<int> randvals;
    //the next 2 methods were pretty much taken exactly
    //from my lab 2 since functionality is pretty much
    //the same
    //offset to return the next valid random number
    //initialization
    int ofs=0;
    //reads the random file of integers takes in the string
    //representing the name of the random integer file
    void rand_numbers(const std::string& rand_file)
    {
        //intitialize the rand file
        std::ifstream rand_input(rand_file);
        //get the total # of random integers and intialize it
        int total_randoms;
        rand_input>>total_randoms;
        //intialize the size of our list that will hold the integers
        randvals.resize(total_randoms);
        //intitialize the actual list with the numbers from the file
        for(int i=0;i<total_randoms;++i)
        {
            rand_input >> randvals[i];
        }
    }
    //returns the next appropriate random integer
    int myrandom(int burst)
    {
        ofs= ofs % randvals.size();
        return randvals[ofs++] % burst;
    }
    //page table entry struct
    struct PTE
    {
        //indicates if entry was loaded to mem or not 1 bit needed only
        unsigned int present:1;
        //indicates if entry was accesssed or not 1 bit needed only
        unsigned int referened:1;
        //indicates if entry was modified or not 1 bit needed only
        unsigned int modified:1;
        //indicates if page was swapped/paged out of mem only 1 bit needed
        unsigned int paged_out:1;
        //indicates # of frames where max was 128 according to instructions so 7 bits needed
        unsigned int frame:7;
        //indicates vma only 1 bit needed
        unsigned int vma_flag:1;
        //indicates if entry is write protected or not only 1 bit needed
        unsigned int write_protect:1;
        //indicates if entry is file mapped or not only 1 bit needed
        unsigned int file_mapped:1;
        //indicates if entry is valid or not only 1 bit needed
        unsigned int valid:1;
        //constructor to ensure all fields are intialized to 0 as mentioned in instructions
        PTE() : present(0), referened(0), modified(0), paged_out(0), frame(0),
                vma_flag(0), write_protect(0), file_mapped(0), valid(0) {}
    };
    //struct representing virtual mem address space
    //as indicated in instructions this contains 4 components from the input file
    struct VMA
    {
        //these are the 4 components of the vma needed for the lab
        //this represent starting vpage 6 bits since max pages is 64 so can be any one of thjose
        unsigned int vpg_start:6;
        //this indicates ending vpage 6 bits as well for same reason
        unsigned int vpg_end:6;
        //this indicates if its write protected
        unsigned int write_protect:1;
        //this indicates if file_mapped
        unsigned int file_mapped:1;
    };
    //struct representing a individual frame
    struct Frame
    {
        //represents if frame is mapped or not only 1 bit needed
        unsigned int mapped:1;
        //represents frame age
        unsigned int age;
        //represents process associated with frame 4 bits since according to instructions
        //max process it will be tested with is 10
        unsigned int pid_helper:4;
        //represents virtual page associated with frame this is 6 bits since max is 64 virtual pages
        //so can be any one of these
        unsigned int vpg_helper:6;
    };
    //made process into class to avoid unnecessary manipulation
    //of page table and vmas of the processes
    class Process{
    private:
        //made our member variables private as this is trivial oop principals
        //to avoid unnecsaarry manipulation from other classes
        std::vector<VMA> vmas;
        std::vector<PTE> page_table;
        //different states/operations a process goes through
        //as from the lab instructions
        unsigned long umaps=0;
        unsigned long maps=0;
        unsigned long ins=0;
        unsigned long fins=0;
        unsigned long outs=0;
        unsigned long fouts=0;
        unsigned long zeroes=0;
        unsigned long segv=0;
        unsigned long segprot=0;
    public:
        //constructor that intializes page table for taht process
        Process() : page_table(max_vpage) {}
        // trivial getter methods for our member variables
        //following oop principals
        unsigned long getUmaps() const { return umaps; }
        unsigned long getMaps() const { return maps; }
        unsigned long getIns() const { return ins; }
        unsigned long getFins() const { return fins; }
        unsigned long getOuts() const { return outs; }
        unsigned long getFouts() const { return fouts; }
        unsigned long getZeroes() const { return zeroes; }
        unsigned long getSegv() const { return segv; }
        unsigned long getSegprot() const { return segprot; }
        std::vector<VMA>& getVMAs() { return vmas; }
        std::vector<PTE>& getPageTable() { return page_table; }
        //increment methods for our states/operations that processes
        //go through in memory management
        void incrementUmaps() { umaps++; }
        void incrementMaps() { maps++; }
        void incrementIns() { ins++; }
        void incrementFins() { fins++; }
        void incrementOuts() { outs++; }
        void incrementFouts() { fouts++; }
        void incrementZeroes() { zeroes++; }
        void incrementSegv() { segv++; }
        void incrementSegprot() { segprot++; }

    };
    //delcaring our frame table
    std::vector<Frame> frame_table;
    //declaring our set of free frames
    std::deque<unsigned int> free_frames;
    //declaring the process list
    std::vector<Process>   process_list;

    //now we will start defining the classes for the actual paging algorithm
    //first we define a base class
    class Pager
    {
    public:
        //trivial constructor
        Pager(){};
        //virtual decstructor for subclasses
        virtual ~Pager(){}
        //age helper method really only used in the aging algorithm
        //and the working set algorithm as needed
        virtual void age(unsigned int frame){}
        //victim method as from lab instructions
        virtual unsigned int victim()=0;
    };
    //first subclass as said in instructions to define is FIFO paging algo
    class FIFO:public Pager
    {
    protected:
        //helper member variable to choose the next victim
        unsigned int index;
    public:
        //trivial constructor
        FIFO(): Pager(), index(0) {}
        //returns the index of the next victim frame
        //relatively trivial
        virtual unsigned int victim()
        {
            //chooses approriate index based on the number of frames and returns it
            index=index % num_frames;
            return index++;
        }
    };
    //implements the Random paging algorithm which uses the rfile
    class RandomPG: public Pager
    {
    public:
        //constructor which takes in the string of the name of the rfile
        //and intializes our list of random numbers
        explicit RandomPG(const std::string& rfile): Pager() { rand_numbers(rfile);}
        //selects the next victim as according to the instructions
        //essentially just gets the next random integer based on the # of frames
        unsigned int victim()
        {
            return myrandom(num_frames);
        }
    };
    //implements the clock paging algorithm
    class Clock: public FIFO
    {
    public:
        //trivial constructor
        //subclass of FIFO since as said in instructions it is a derivative of FIFO algorithm
        Clock(): FIFO() {}
        //selects the next victim as mentioned in the lab instructions
        //its pretty much same as FIFO but now we check for reference bits and resetting it
        unsigned int victim()
        {
            while(true)
            {
                //get the entry of the frame selected by the index
                unsigned int pid_helper=frame_table[index].pid_helper;
                unsigned int vpg_helper=frame_table[index].vpg_helper;
                PTE& pte_helper= process_list[pid_helper].getPageTable()[vpg_helper];
                //check if the entry has been referenced
                if(pte_helper.referened)
                {
                    //if so set it to false
                    pte_helper.referened=false;
                    //wrap around our index variable
                    index= (index+1) % num_frames;
                }
                else
                {
                    //if it hasn't been referred we can exit the loop
                    //since we found a valid frame
                    break;
                }
            }
            //assign the victim
            unsigned int victim = index;
            //increment the index appropriately and return the victim
            index= (index+1) % num_frames;
            return victim;
        }
    };
    //implements the ESC/NRU paging algorithm FIFO is baseclass
    class Esc: public FIFO
    {
    private:
        //we have to consider our instruction bound for this algorithm
        //essentially after 48 instructions since the last reset of the
        //reference bit have passed then we must clar the reference bit
        //of the pte described in the instrutions
        //so we created 2 helper vairables one too maintain the bound of the reset
        //since we don't want to hard code 48, and another to keep track of the last
        //reset
        static const unsigned int reset=48;
        unsigned long last_reset;
    public:
        //trivial constructor intializes our member variable for last reset
        Esc(): FIFO(), last_reset(-1){}
        //
        unsigned int victim()
        {
            //keep track of the best frame to be the victim
            unsigned int best_frame = index;
            //keep track of the best level set as maximum which would be invalid level
            int best_level = 4;
            //keep track of index we started at for frame retrieval
            unsigned int start=index;
            //see if we need to reset or not based on the algebra mentioned
            //in the lab instructions: if # of instructions since last
            //reset is greater or equal to 48
            bool reset_flag = instruction_count - last_reset >= reset;
            do
            {
                //reverse map to to get the appropriate page table entry
                //of the current frame by first getting proc id and vpage of curr frame
                //then getting the entry of the frame
                unsigned int pid_helper=frame_table[index].pid_helper;
                unsigned int vpg_helper=frame_table[index].vpg_helper;
                PTE& pte_helper= process_list[pid_helper].getPageTable()[vpg_helper];
                //determine levle based on R/M bits
                int lvl= pte_helper.referened*2 + pte_helper.modified;
                //if level hasn't been recorded record it
                if (lvl < best_level)
                {
                    best_level = lvl;
                    best_frame = index;
                    //if best level is 0 and we don't need to reset we can exit the loop
                    //since valid frame has been found as according to lab instructions
                    if (best_level == 0 && !reset_flag)
                    {
                        break;
                    }
                }
                //if reset is needed perform the reset on the refernece bit
                if(reset_flag)
                {
                    pte_helper.referened=false;
                }
                //wrap aroudn to choose the next index
                index= (index+1) % num_frames;
            } while(index!=start);
            //if we reset the reference bit keep track of when we reset so it can be used
            //later to keep track of the last reset based on the instruction count it occured on
            if(reset_flag)
            {
                last_reset=instruction_count;
            }
            //wrap around the index
            index=(best_frame+1) % num_frames;
            //return the victim
            return best_frame;
        }
    };
    //implements the aging algorithm
    class Aging: public FIFO
    {
    public:
        //trivial constructor
        Aging(): FIFO(){}
        // resets the age to 0 as needed for MAP operations
        //as mentioned in the instruction
        void age(unsigned int frame)
        {
            frame_table[frame].age=0;
        }

        unsigned int victim()
        {
            //keeps track of index started when seraching for frames
            unsigned int start=index;
            //keep track of current best frame for victim assuming current index is best
            unsigned int best_frame = index;
            //keep track of the best age initalizing with max possible age sicne thats the worst age
            uint32_t best_age = UINT32_MAX;

            do
            {
                //reverse map from frame to get associated entry
                //get its proc id and vpage then access page table to get
                //the appropriate entry
                unsigned int pid_helper=frame_table[index].pid_helper;
                unsigned int vpg_helper=frame_table[index].vpg_helper;
                PTE& pte_helper= process_list[pid_helper].getPageTable()[vpg_helper];
                //implement the aging as said in the instructions it must be
                //implemented on every page replacement request
                frame_table[index].age >>=1;
                //if it was referenced before
                if(pte_helper.referened)
                {
                    //we set leading bit to 1
                    frame_table[index].age |= 0x80000000;
                    //set reference to false
                    pte_helper.referened=false;
                }
                //choose the frame with the lowest age
                if(frame_table[index].age<best_age)
                {
                    best_frame = index;
                    best_age = frame_table[index].age; // Update the smallest age
                }
                //wrap around our victim index
                index= (index+1) % num_frames;
            } while(index!=start);
            //wrap around our index as according to teh actual victim index
            //chosen as described in the lab instructions and return the victim int
            index=(best_frame+1) % num_frames;
            return best_frame;
        }
    };
    //implementes working set paging algorithm
    class WorkingSet: public FIFO
    {
    private:
        //create some helper member variables
        //as mentioned in lab instructions we are assumign TAU=49
        //then if 50 or more instructions have been passed since last use
        //was recorded for the frame and reference bit isn''t set we choose
        // that frame
        static const unsigned int tau_helper=49;
        //keeps track of the last use
        std::vector<unsigned long> recent_used;
    public:
        //trivial constructor
        WorkingSet(): FIFO(), recent_used(num_frames){}
        //we implement the age method although it is not using the aging concept
        //we really just use it for keeping track of the last use of a frame
        void age(unsigned int frame)
        {
            recent_used[frame]=instruction_count;
        }
        //implement selecting the next victim
        unsigned int victim()
        {
            //keeps track of index we start at
            unsigned int start=index;
            //keeps trakc of the recently chosen frame
            unsigned int recent=index;
            do
            {
                //reverse map the frame to get the entry by
                //first getting its associed proc id and vpage
                //then access the page table to get the entry
                unsigned int pid_helper=frame_table[index].pid_helper;
                unsigned int vpg_helper=frame_table[index].vpg_helper;
                PTE& pte_helper= process_list[pid_helper].getPageTable()[vpg_helper];
                //check if its has been referenced
                if(pte_helper.referened)
                {
                    //it can't be used since referenced
                    //so update its last use and set referenced to false
                    recent_used[index]=instruction_count;
                    pte_helper.referened=false;
                }
                //as said in the instructions if more than 49 instructions were passed
                //and entry was not referneced we use that frame as the victim
                else if(instruction_count-recent_used[index]>tau_helper)
                {
                    //set the index of the frame chosen and break oput of the loop
                    recent=index;
                    break;
                }
                //check if the actual index tracker was used before the recent index
                //tracker since if so then we use the frame that was used earlier
                if(recent_used[index]<recent_used[recent])
                {
                    recent=index;
                }
                //wrap around our index variable
                index= (index+1) % num_frames;
            }while(index != start);
            //wrap around the index variable according to frame chosen and return chosen frame
            index= (recent+1) % num_frames;
            return recent;
        }
    };
    //prints the frame table used in output if option f is passed
    void printFrameTable(const std::vector<Frame>& frame_tb)
    {
        //essentialy follows the format as described in the lab instructions
        //initial printing representign we are print the frame table
        std::cout<< "FT:";
        //iterate through the frame table
        for(int i=0;i<frame_tb.size();i++)
        {
            //get the frame
            const Frame& frame=frame_tb[i];
            //if mapped do the necessary output
            if(frame.mapped)
            {
                std::cout << " " << frame.pid_helper << ":" << frame.vpg_helper;
            }
            else
            {
                //if not give the approriate output
                std::cout<< " *";
            }
        }
        //end the line for the output
        std::cout<< std::endl;
    }
    //prints the page table if option p is passsed
    void printPageTable()
    {
        //loops through each process to access their respective page tables
        for (int i = 0; i < process_list.size(); i++) {
            //gets the process
            Process& proc = process_list[i];
            //intial printing for the representing process chosen as according to
            //lab instructions
            std::cout << "PT[" << i << "]:";
            //iterate through the process page table
            for(int i=0;i<proc.getPageTable().size();i++)
            {
                //get the entry
                const PTE &pte = proc.getPageTable()[i];
                //if entry is present do necessary printing based on R/M/paged_out bits
                //as described in lab instructions
                if(pte.present)
                {
                    std::cout << " " << i << ":"
                            << (pte.referened ? "R" : "-")
                            << (pte.modified ? "M" : "-")
                            << (pte.paged_out ? "S" : "-");
                }
                else
                {
                    //if not present we print # or * based on if it was paged out or not
                    std::cout<< (pte.paged_out ? " #" : " *");
                }
            }
            //end the line for the current process page table that was just printed
            std::cout<<std::endl;
        }
    }
    //if option s was passed we must print the processes
    void printProcesses()
    {
        //loop through the processes
        for (int i = 0; i < process_list.size(); i++) {
            //select the proccess
            Process& proc = process_list[i];
            //do the intitial printing representing the process selected
            std::cout << "PROC[" << i << "]:";
            //print all the necessary properties of the process as
            //according to the lab instructions
            std::cout<< " U=" << proc.getUmaps()
                     << " M=" << proc.getMaps()
                     << " I=" << proc.getIns()
                     << " O=" << proc.getOuts()
                     << " FI=" << proc.getFins()
                     << " FO=" << proc.getFouts()
                     << " Z=" << proc.getZeroes()
                     << " SV=" << proc.getSegv()
                     << " SP=" << proc.getSegprot();
            //end the line for the print of that process
            std::cout<<std::endl;
        }
    }
    //create global variable rerpesentign page_algorithm used
    Pager * page_algorithm= nullptr;
    //create global booleans representing the options that may be passed
    //we don't include f, a, x, or y since according to lab instructions those
    //wouldn't be tested
    bool o=false;
    bool p=false;
    bool f=false;
    bool s=false;
    //global variable to keep track of nextline when parsing actual input file
    std::stringstream nextline;
    //global variable that will hold the actual inputfile
    std::ifstream infile;
    //I didn't want to hardcode the cost of each operation when adding to the total cost
    //so i created a cost enum that is kind of like a cost table but its an enum for
    //optimization. essentially intialize cost of each operation as according to lab instructions
    enum Cost
    {
        R_W=1,
        SWITCHES=130,
        EXITS=1230,
        MAPS=350,
        UNMAPS=410,
        INS=3200,
        OUTS=2750,
        FINS=2350,
        FOUTS=2800,
        ZEROS=150,
        SEGV=440,
        SEGPROT=410
    };
    //method to allocate a frame
    unsigned int allocate(unsigned int vpage)
    {
        //helper variable to hold free frame int
        unsigned int free;
        //check if free frame avaialble and if so
        //get the free frame and remove it from list of free frames
        if(!free_frames.empty())
        {
            free=free_frames.front();
            free_frames.pop_front();
        }
        else
        {
            //if no free frames avaiable
            //then choose the next frame victim according to paging algorithm
            free= page_algorithm->victim();
            //reverse map the frame to ge tthe entry by first getting proc id and vpg associated with frame
            //and getting the entry from the proc
            unsigned int temp_pid=frame_table[free].pid_helper;
            unsigned int temp_vpg= frame_table[free].vpg_helper;
            Process& temp_proc= process_list[temp_pid];
            PTE& pte_helper=temp_proc.getPageTable()[temp_vpg];
            //if option o do necessary printing for UNMAP operation
            if(o)
            {
                std::cout<< " UNMAP " << temp_pid << ":" << temp_vpg << std::endl;
            }
            //do accounting for unmap operation for the total cost global variable
            temp_proc.incrementUmaps();
            total_cost+=Cost::UNMAPS;
            //set entry to as not present
            pte_helper.present=false;
            //check if it was modified
            if(pte_helper.modified)
            {
                //if it wasn't filed mapped then its paged out so do necessary check
                pte_helper.paged_out = !pte_helper.file_mapped;
                //if option o was passed do necessary printing for page out
                if(o)
                {
                    std::cout << (pte_helper.file_mapped ? " FOUT" : " OUT") << std::endl;
                }
                //if it was file mapped then do account for fouts
                if(pte_helper.file_mapped)
                {
                    temp_proc.incrementFouts();
                    total_cost+=Cost::FOUTS;
                }
                else
                {
                    //if it waasn't file mapped just do accounting for outs
                    temp_proc.incrementOuts();
                    total_cost+=Cost::OUTS;
                }
            }

        }
        //for the frame that was chosen as victim set the properties
        //which are pid, vpage, and mapped then return the frame integer
        frame_table[free].pid_helper=curr_pid;
        frame_table[free].vpg_helper=vpage;
        frame_table[free].mapped=true;
        return free;
    }
    //page fault handler
    void page_fault(PTE& pte, unsigned int vpage)
    {
        //if the entry has not been accessed yet
        if(!pte.vma_flag)
        {
            //mark it as checked
            pte.vma_flag=true;
            //loop through all vmas of the current process
            for(VMA& vma: process_list[curr_pid].getVMAs())
            {
                //check if the virtual page is within bounds of the current virtual page
                if(vpage>= vma.vpg_start && vpage <= vma.vpg_end)
                {
                    //set PTE fields according to the vma properties from the input
                    pte.valid=true;//sets the entry as valid
                    pte.file_mapped=vma.file_mapped;//same property as vma
                    pte.write_protect=vma.write_protect;//same property as vma
                    break;
                }
            }
        }
        //if entry is valid
        if(pte.valid)
        {
            //entry is present in memory
            pte.present=true;
            //allocate frame for the entry
            pte.frame=allocate(vpage);
            //handle the case of entry being paged out
            if(pte.paged_out)
            {
                //if option o was passed do necessary printing as according to lab
                //instructions indicating page is back in
                if(o)
                {
                    std::cout << " IN"<< std::endl;
                }
                //update the insertions for current process
                process_list[curr_pid].incrementIns();
                //update our global cost variable used for output according to
                //cost from lab instructions
                total_cost+=Cost::INS;
            }
            //if it is file-mapped
            else if(pte.file_mapped)
            {
                //do necessary prnting as according to lab instruction if option o is passed
                if(o)
                {
                    std::cout<< " FIN"<< std::endl;
                }
                //updated the process field counter
                process_list[curr_pid].incrementFins();
                //update our total cost vairable used for output later on
                total_cost+=Cost::FINS;
            }
            else
            {
                //page is new so doe necessay zeros printing as according to
                //lab instructions
                if(o)
                {
                    std::cout << " ZERO" << std::endl;
                }
                //update our process field counter for new page
                process_list[curr_pid].incrementZeroes();
                //update our total cost variable according to cost table
                //as gotten from lab instructions
                total_cost+=Cost::ZEROS;
            }
            //account for mapping operation so do necessary printing
            if(o)
            {
                std::cout << " MAP " << pte.frame << std::endl;
            }
            //update process field map counter
            process_list[curr_pid].incrementMaps();
            //update global cost vairable needed for output later
            total_cost+=Cost::MAPS;
            //perform page age operation
            page_algorithm->age(pte.frame);
        }
        else
        {
            //if page was invalid then, its not part of vma
            //so its a violation
            //do necessary printing for this as according to lab instructions
            if(o)
            {
                std::cout<< " SEGV" << std::endl;
            }
            //update process field counter for violation
            process_list[curr_pid].incrementSegv();
            //updated global cost vairable according to cost table
            total_cost+=Cost::SEGV;
        }
    }
    //this method is a helper method for parsing the actual input file
    //this method gets the next line from the input file until a valid line is found
    bool get_valid_line()
    {
        std::string line;
        while(getline(infile, line))
        {
            //a valid line is a line which doesn't start with a # in our case
            //so if line starts with # we continue to get the next line
            if(line[0]=='#')
            {
                continue;
            }
            //if the line doesn't start with a # it is a valid line we should read
            //so clear the current contents of our next line global vairable
            //and assign it to the valid line as a string
            nextline.clear();
            nextline.str(line);
            //returns true since valid line was found
            return true;
        }
        //if valid line wasn't found i return false
        return false;
    }
    //used to get the actual instructions once we have parsed
    //through the prelimnary intialization parts of the input file
    bool get_instruction(char& operation, int& vpage_proc)
    {
        //essentially first checks for valid line
        if (get_valid_line())
        {
            //if present we get the operation character and the vpage/proc id integer
            //associated with the instruction
            nextline >> operation >> vpage_proc;
            //returns true since valid instruction line is found
            return true;
        }
        //if wasn't found we return false
        return false;
    }
    //I avoided switch case statements in the actual simulation of instructions
    //as said in the instructions of the lab and used oop concepts instead for
    //the actual page replacement code
    //switch case here is only used to read in the instruction which can be
    //c, r, w, or e
    void simulation(std::string filename) {
        //to start the simulation we parse the actual inputfile
        //first we open the file
        infile.open(filename);
        //then we check for a valid line which should be present
        //if not input file is invalid so we terminate
        if (!get_valid_line()) {
            std::cerr << "Invalid input file";
            exit(1);
        }
        //the first valid line in all inputfiles should be the number of processes
        //so we create a helper vairable to represent this number of processes
        int num_proc;
        nextline >> num_proc;
        //loop through the number of processes
        while (num_proc-- > 0)
        {
            //create each process and add it to our pool of processes
            process_list.push_back(Process());
            //now we move on to the next line
            //if not present then our input file is invalid and we terminate
            if (!get_valid_line()) {
                std::cerr << "Invalid input";
                exit(1);
            }
            //the next valid line should represent  the # of vmas for the individual process
            //so we create a helper variable to store this
            int num_vmas;
            nextline >> num_vmas;
            //we loop through each vma of the process
            while(num_vmas-- >0)
            {
                //get the next valid line if not there we have invalid input and terminate
                if(!get_valid_line())
                {
                    std::cerr<<"Invalid input";
                    exit(1);
                }
                //this valid line now represents the 4 components of the vma as mentioned in the
                //lab instructions so we create the helper variables to store these 4 components
                unsigned int vpg_start, vpg_end;
                bool write_protect, file_mapped;
                nextline >> vpg_start
                         >> vpg_end
                         >> write_protect
                         >> file_mapped;
                //now we have all the components we intialize the vma
                VMA address{vpg_start, vpg_end, write_protect, file_mapped};
                //then we add the vma of the process to its list of vmas
                process_list.back().getVMAs().push_back(address);
            }
        }
        //now we have gone through all process intialization and their repsepctive
        //vmas we move on to the actual instructions

        //now the actual instruction line consists of the instruction character
        // and then its respective integer which can be vpage or procid so I just
        //called it vpage_proc
        int vpage_proc;
        char operation;
        //we get the next instruction intiailizing the operation and vpage/proc integer
        while(get_instruction(operation,vpage_proc))
        {
            //if option o was passed in the o argument flag
            //we print the necessary output as according to the lab instructions
            if(o)
            {
                std::cout << instruction_count << ": ==> "
                          << operation << " "
                          << vpage_proc << std::endl;
            }
            //as mentioned we only use switch case here to loop through the different operation types
            //since this is more optimal than a if-else statement
            //I wasn't sure how else to do it
            switch(operation)
            {
                //if w instruction we don't do anything
                //explicitly instead we group it with the r case since the cost
                //of a w and r instruction is the same
                //so if the operation is 'w' then it executes the 'r' block case statement
                case 'w':
                //as mentioned above this case is executed if the operations is 'r' or 'w'
                case 'r':
                {
                    //we get the page table entry of the current process corresponding to the vpage
                    //integer that was passed with the instruction
                    PTE& pte=process_list[curr_pid].getPageTable()[vpage_proc];
                    //set the entrys referenced flag to true
                    pte.referened=true;
                    //check our present flag which is used to indicate if page fault will occur or not
                    if(!pte.present)
                    {
                        //if not present we set modified to false and must address the page fault
                        pte.modified=false;
                        //address the page fault
                        page_fault(pte,vpage_proc);
                    }
                    //if it is a write operation and the entry is write protected according to its flag
                    if(operation=='w' && pte.write_protect)
                    {
                        //if option o was present in the arguments then print the necessary
                        //protection output
                        if(o)
                        {
                            std::cout<< " SEGPROT" << std::endl;
                        }
                        //increment our protection field for the current process
                        process_list[curr_pid].incrementSegprot();
                        //increment the total cost according to the cost of protection as mentioned
                        //in the lab instructions
                        total_cost+=Cost::SEGPROT;
                    }
                    else
                    {
                        //bit assignment of our modified field
                        //essentially we set modified to itself if read instruction or to true
                        //if write instruction
                        pte.modified = pte.modified || (operation == 'w');
                    }
                    //increment our total_cost variable used in our output according to the cost
                    //of a read or write instruction from our lab instructions
                    total_cost+=Cost::R_W;
                    break;
                }
                case 'c':
                {
                    //this case implies the instructions is a context switch
                    //first we increment our global context_switch counter used for output
                    context_switch++;
                    //then we reset the current process id to the proc id that was passed with the instruction
                    //since we are now switching
                    curr_pid=vpage_proc;
                    //finally increment the total cost variable used for output according to our costs
                    //that we got from the lab instructions
                    total_cost+=Cost::SWITCHES;
                    break;
                }
                case 'e':
                {
                    //this instruction implies we exit the process whose proc id is given
                    //so first we do our necessary print as accoridng to lab instructions
                    std::cout<< "EXIT current process " << vpage_proc << std::endl;
                    //we increment our global process_exits variable used for output
                    process_exits++;
                    //get the process object from our process pool
                    Process& proc=process_list[vpage_proc];
                    //we go through the page table of the process exiting
                    for(int i=0;i<proc.getPageTable().size();i++)
                    {
                        //ge the each individual page table entry
                        PTE& pte= proc.getPageTable()[i];
                        //set the paged out flag
                        pte.paged_out=false;
                        //if present in page table we can now set it to false
                        if(pte.present)
                        {
                            pte.present=false;
                            //if option o was passed as an argument
                            //we do our necessary output as according to instruction for unmaping the
                            //entry
                            if(o)
                            {
                                std::cout << " UNMAP " << vpage_proc << ":" << i << std::endl;
                            }
                            //we increment our variable for the number of unmaps done for the individual process
                            proc.incrementUmaps();
                            //set the frame entry corresponds to mapped field to false
                            frame_table[pte.frame].mapped=false;
                            //frame is now free so we can push that frame onto list of free frames
                            free_frames.push_back(pte.frame);
                            //if the entry was mapped and modified
                            if(pte.file_mapped && pte.modified)
                            {
                                //we do our necesarry output for FOUTS
                                if(o)
                                {
                                    std::cout<< " FOUT" << std::endl;
                                }
                                //increment our process field for number of fouts
                                proc.incrementFouts();
                                //increment our total cost global variable used for output
                                //with the corresponding cost as given in lab instructions
                                total_cost+=Cost::FOUTS;
                            }
                            //increment our total cost global variable used in output
                            //as given in lab instructions
                            total_cost+=Cost::UNMAPS;
                        }
                    }
                    //increment our total cost global variable used in output
                    //with cost of process exiting as given in lab instructions
                    total_cost+=Cost::EXITS;
                }
            }
            //we increment our instruction count variable used in output
            instruction_count++;
        }
    }
}
int main(int argc, char * argv[]) {
//check that all the necessary arguments are present
    if (argc < 6) {
        std::cerr << "Usage: ./mmu -f<num_frames> -a<algo> inputfile\n";
        return 1;
    }
    //create helper variables
    int arg;//represents the individual arguments passed in
    char algoIndicator = 'f';//represents the algorithm being used
    //initialized to fifo but is updated later based on the argument

    //i parsed the input as said in the instructions using getopt and optarg
    while ((arg = getopt(argc, argv, "f:a:o:")) != -1) {
        //parsing through the f, a, and o argument flags
        //I try to use switch cases as much as I can
        //since as we learned from the last lab it is much more optimized
        //compared to an if-else statements
        switch (arg) {
            case 'f':
                //f argument represents number of frames so use stoi to
                //intialize our global variable num_frames
                mmu::num_frames = std::stoi(optarg);
                break;
            case 'a':
                //a argument represent the algorithm that will be used
                //for paging
                algoIndicator = std::string(optarg)[0];
                break;
            case 'o': {
                //o represents the different options

                std::string option = std::string(optarg);
                for (char &individual: option) {
                    //according to the instruction the only options
                    //tested are O, P, F, S so I only implemented those 4
                    switch (individual) {
                        case 'O':
                            mmu::o = true;
                            break;
                        case 'P':
                            mmu::p = true;
                            break;
                        case 'F':
                            mmu::f = true;
                            break;
                        case 'S':
                            mmu::s = true;
                            break;
                            //since I only implmeneted those 4 since these are the only
                            //options tested according to the instructions if other options are passed
                            //it will go into this default clause and terminate
                            //so it should only be tested with the 4 above required options
                        default:
                            std::cerr << "Invalid option: " << individual << '\n';
                            return 1;
                    }
                }
                break;
            }
            //if any other option arguments are passed it is invalid so program will
            //terminate
            default:
                std::cerr << "Invalid command arguments" << '\n';
                return 1;
        }
    }
    //read in the file name for the input
    std::string filename = argv[4];
    //initialize our frame table and our list of free frames
    //according to the number of frames that was passed in the
    //f argument flag
    for (int i = 0; i < mmu::num_frames; i++) {
        //creates frame and pushes it onto the frame_table
        mmu::frame_table.push_back(mmu::Frame());
        //creates a free frame since we are just intializing
        //and all start as free frames
        mmu::free_frames.push_back(i);
    }
    //creating the actual paging algorithm object according to the argument
    //that was passed from the a flag. using switch case again for
    //optimization
    switch (algoIndicator) {
        case 'f':
            mmu::page_algorithm = new mmu::FIFO();
            break;
        case 'c':
            mmu::page_algorithm = new mmu::Clock();
            break;
        case 'a':
            mmu::page_algorithm = new mmu::Aging();
            break;
        case 'e':
            mmu::page_algorithm = new mmu::Esc();
            break;
        case 'w':
            mmu::page_algorithm = new mmu::WorkingSet();
            break;
        case 'r':
            mmu::page_algorithm = new mmu::RandomPG(argv[5]);
            break;
        default:
            std::cerr << "Invalid algorithm argument";
            return 1;
    }
    //start the simulation
    mmu::simulation(filename);
    //simulation is now over so we can delete the paging algorithm object
    delete mmu::page_algorithm;
    //print the necessary output according to the different options
    //that were passed with the o argument flag


    if (mmu::p)
    {
        //if option p is passed we just print the page table of each process
        mmu::printPageTable();
    }

    if (mmu::f) {
        //if option f is passed we just print the frame table
        mmu::printFrameTable(mmu::frame_table);
    }

    if (mmu::s) {
        //if option s is passed we print the process along with total cost
        mmu::printProcesses();
        //total costs consists of  total # of instructions,
        //total # of context switches, total # of process exits, and the actual
        //total cost which is summed up according to the individual cost from the instructions
        std::cout << "TOTALCOST " << mmu::instruction_count << " "
                  << mmu::context_switch << " "
                  << mmu::process_exits << " "
                  << mmu::total_cost << " "
                  << sizeof(mmu::PTE) << std::endl;
    }
    return 0;
}