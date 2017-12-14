/* config.js
 * 
 * This file contains the code necessary to load the configuration
 * for the experiment.
 */

// Object to hold the experiment configuration. It takes as parameters
// the numeric codes representing the experimental condition and
// whether the trials are counterbalanced.
var Config = function (condition, counterbalance) {

    // maleNames taken from https://www2.census.gov/topics/genealogy/1990surnames/dist.male.first
    // 1219 entries
    var maleNames = ['James', 'John', 'Robert', 'Michael', 'William', 'David', 'Richard', 'Charles', 'Joseph', 'Thomas', 'Christopher', 'Daniel', 'Paul', 'Mark', 'Donald', 'George', 'Kenneth', 'Steven', 'Edward', 'Brian', 'Ronald', 'Anthony', 'Kevin', 'Jason', 'Matthew', 'Gary', 'Timothy', 'Jose', 'Larry', 'Jeffrey', 'Frank', 'Scott', 'Eric', 'Stephen', 'Andrew', 'Raymond', 'Gregory', 'Joshua', 'Jerry', 'Dennis', 'Walter', 'Patrick', 'Peter', 'Harold', 'Douglas', 'Henry', 'Carl', 'Arthur', 'Ryan', 'Roger', 'Joe', 'Juan', 'Jack', 'Albert', 'Jonathan', 'Justin', 'Terry', 'Gerald', 'Keith', 'Samuel', 'Willie', 'Ralph', 'Lawrence', 'Nicholas', 'Roy', 'Benjamin', 'Bruce', 'Brandon', 'Adam', 'Harry', 'Fred', 'Wayne', 'Billy', 'Steve', 'Louis', 'Jeremy', 'Aaron', 'Randy', 'Howard', 'Eugene', 'Carlos', 'Russell', 'Bobby', 'Victor', 'Martin', 'Ernest', 'Phillip', 'Todd', 'Jesse', 'Craig', 'Alan', 'Shawn', 'Clarence', 'Sean', 'Philip', 'Chris', 'Johnny', 'Earl', 'Jimmy', 'Antonio', 'Danny', 'Bryan', 'Tony', 'Luis', 'Mike', 'Stanley', 'Leonard', 'Nathan', 'Dale', 'Manuel', 'Rodney', 'Curtis', 'Norman', 'Allen', 'Marvin', 'Vincent', 'Glenn', 'Jeffery', 'Travis', 'Jeff', 'Chad', 'Jacob', 'Lee', 'Melvin', 'Alfred', 'Kyle', 'Francis', 'Bradley', 'Jesus', 'Herbert', 'Frederick', 'Ray', 'Joel', 'Edwin', 'Don', 'Eddie', 'Ricky', 'Troy', 'Randall', 'Barry', 'Alexander', 'Bernard', 'Mario', 'Leroy', 'Francisco', 'Marcus', 'Micheal', 'Theodore', 'Clifford', 'Miguel', 'Oscar', 'Jay', 'Jim', 'Tom', 'Calvin', 'Alex', 'Jon', 'Ronnie', 'Bill', 'Lloyd', 'Tommy', 'Leon', 'Derek', 'Warren', 'Darrell', 'Jerome', 'Floyd', 'Leo', 'Alvin', 'Tim', 'Wesley', 'Gordon', 'Dean', 'Greg', 'Jorge', 'Dustin', 'Pedro', 'Derrick', 'Dan', 'Lewis', 'Zachary', 'Corey', 'Herman', 'Maurice', 'Vernon', 'Roberto', 'Clyde', 'Glen', 'Hector', 'Shane', 'Ricardo', 'Sam', 'Rick', 'Lester', 'Brent', 'Ramon', 'Charlie', 'Tyler', 'Gilbert', 'Gene', 'Marc', 'Reginald', 'Ruben', 'Brett', 'Angel', 'Nathaniel', 'Rafael', 'Leslie', 'Edgar', 'Milton', 'Raul', 'Ben', 'Chester', 'Cecil', 'Duane', 'Franklin', 'Andre', 'Elmer', 'Brad', 'Gabriel', 'Ron', 'Mitchell', 'Roland', 'Arnold', 'Harvey', 'Jared', 'Adrian', 'Karl', 'Cory', 'Claude', 'Erik', 'Darryl', 'Jamie', 'Neil', 'Jessie', 'Christian', 'Javier', 'Fernando', 'Clinton', 'Ted', 'Mathew', 'Tyrone', 'Darren', 'Lonnie', 'Lance', 'Cody', 'Julio', 'Kelly', 'Kurt', 'Allan', 'Nelson', 'Guy', 'Clayton', 'Hugh', 'Max', 'Dwayne', 'Dwight', 'Armando', 'Felix', 'Jimmie', 'Everett', 'Jordan', 'Ian', 'Wallace', 'Ken', 'Bob', 'Jaime', 'Casey', 'Alfredo', 'Alberto', 'Dave', 'Ivan', 'Johnnie', 'Sidney', 'Byron', 'Julian', 'Isaac', 'Morris', 'Clifton', 'Willard', 'Daryl', 'Ross', 'Virgil', 'Andy', 'Marshall', 'Salvador', 'Perry', 'Kirk', 'Sergio', 'Marion', 'Tracy', 'Seth', 'Kent', 'Terrance', 'Rene', 'Eduardo', 'Terrence', 'Enrique', 'Freddie', 'Wade', 'Austin', 'Stuart', 'Fredrick', 'Arturo', 'Alejandro', 'Jackie', 'Joey', 'Nick', 'Luther', 'Wendell', 'Jeremiah', 'Evan', 'Julius', 'Dana', 'Donnie', 'Otis', 'Shannon', 'Trevor', 'Oliver', 'Luke', 'Homer', 'Gerard', 'Doug', 'Kenny', 'Hubert', 'Angelo', 'Shaun', 'Lyle', 'Matt', 'Lynn', 'Alfonso', 'Orlando', 'Rex', 'Carlton', 'Ernesto', 'Cameron', 'Neal', 'Pablo', 'Lorenzo', 'Omar', 'Wilbur', 'Blake', 'Grant', 'Horace', 'Roderick', 'Kerry', 'Abraham', 'Willis', 'Rickey', 'Jean', 'Ira', 'Andres', 'Cesar', 'Johnathan', 'Malcolm', 'Rudolph', 'Damon', 'Kelvin', 'Rudy', 'Preston', 'Alton', 'Archie', 'Marco', 'Wm', 'Pete', 'Randolph', 'Garry', 'Geoffrey', 'Jonathan', 'Felipe', 'Bennie', 'Gerardo', 'Ed', 'Dominic', 'Robin', 'Loren', 'Delbert', 'Colin', 'Guillermo', 'Earnest', 'Lucas', 'Benny', 'Noel', 'Spencer', 'Rodolfo', 'Myron', 'Edmund', 'Garrett', 'Salvatore', 'Cedric', 'Lowell', 'Gregg', 'Sherman', 'Wilson', 'Devin', 'Sylvester', 'Kim', 'Roosevelt', 'Israel', 'Jermaine', 'Forrest', 'Wilbert', 'Leland', 'Simon', 'Guadalupe', 'Clark', 'Irving', 'Carroll', 'Bryant', 'Owen', 'Rufus', 'Woodrow', 'Sammy', 'Kristopher', 'Mack', 'Levi', 'Marcos', 'Gustavo', 'Jake', 'Lionel', 'Marty', 'Taylor', 'Ellis', 'Dallas', 'Gilberto', 'Clint', 'Nicolas', 'Laurence', 'Ismael', 'Orville', 'Drew', 'Jody', 'Ervin', 'Dewey', 'Al', 'Wilfred', 'Josh', 'Hugo', 'Ignacio', 'Caleb', 'Tomas', 'Sheldon', 'Erick', 'Frankie', 'Stewart', 'Doyle', 'Darrel', 'Rogelio', 'Terence', 'Santiago', 'Alonzo', 'Elias', 'Bert', 'Elbert', 'Ramiro', 'Conrad', 'Pat', 'Noah', 'Grady', 'Phil', 'Cornelius', 'Lamar', 'Rolando', 'Clay', 'Percy', 'Dexter', 'Bradford', 'Merle', 'Darin', 'Amos', 'Terrell', 'Moses', 'Irvin', 'Saul', 'Roman', 'Darnell', 'Randal', 'Tommie', 'Timmy', 'Darrin', 'Winston', 'Brendan', 'Toby', 'Van', 'Abel', 'Dominick', 'Boyd', 'Courtney', 'Jan', 'Emilio', 'Elijah', 'Cary', 'Domingo', 'Santos', 'Aubrey', 'Emmett', 'Marlon', 'Emanuel', 'Jerald', 'Edmond', 'Emil', 'Dewayne', 'Will', 'Otto', 'Teddy', 'Reynaldo', 'Bret', 'Morgan', 'Jess', 'Trent', 'Humberto', 'Emmanuel', 'Stephan', 'Louie', 'Vicente', 'Lamont', 'Stacy', 'Garland', 'Miles', 'Micah', 'Efrain', 'Billie', 'Logan', 'Heath', 'Rodger', 'Harley', 'Demetrius', 'Ethan', 'Eldon', 'Rocky', 'Pierre', 'Junior', 'Freddy', 'Eli', 'Bryce', 'Antoine', 'Robbie', 'Kendall', 'Royce', 'Sterling', 'Mickey', 'Chase', 'Grover', 'Elton', 'Cleveland', 'Dylan', 'Chuck', 'Damian', 'Reuben', 'Stan', 'August', 'Leonardo', 'Jasper', 'Russel', 'Erwin', 'Benito', 'Hans', 'Monte', 'Blaine', 'Ernie', 'Curt', 'Quentin', 'Agustin', 'Murray', 'Jamal', 'Devon', 'Adolfo', 'Harrison', 'Tyson', 'Burton', 'Brady', 'Elliott', 'Wilfredo', 'Bart', 'Jarrod', 'Vance', 'Denis', 'Damien', 'Joaquin', 'Harlan', 'Desmond', 'Elliot', 'Darwin', 'Ashley', 'Gregorio', 'Buddy', 'Xavier', 'Kermit', 'Roscoe', 'Esteban', 'Anton', 'Solomon', 'Scotty', 'Norbert', 'Elvin', 'Williams', 'Nolan', 'Carey', 'Rod', 'Quinton', 'Hal', 'Brain', 'Rob', 'Elwood', 'Kendrick', 'Darius', 'Moises', 'Son', 'Marlin', 'Fidel', 'Thaddeus', 'Cliff', 'Marcel', 'Ali', 'Jackson', 'Raphael', 'Bryon', 'Armand', 'Alvaro', 'Jeffry', 'Dane', 'Joesph', 'Thurman', 'Ned', 'Sammie', 'Rusty', 'Michel', 'Monty', 'Rory', 'Fabian', 'Reggie', 'Mason', 'Graham', 'Kris', 'Isaiah', 'Vaughn', 'Gus', 'Avery', 'Loyd', 'Diego', 'Alexis', 'Adolph', 'Norris', 'Millard', 'Rocco', 'Gonzalo', 'Derick', 'Rodrigo', 'Gerry', 'Stacey', 'Carmen', 'Wiley', 'Rigoberto', 'Alphonso', 'Ty', 'Shelby', 'Rickie', 'Noe', 'Vern', 'Bobbie', 'Reed', 'Jefferson', 'Elvis', 'Bernardo', 'Mauricio', 'Hiram', 'Donovan', 'Basil', 'Riley', 'Ollie', 'Nickolas', 'Maynard', 'Scot', 'Vince', 'Quincy', 'Eddy', 'Sebastian', 'Federico', 'Ulysses', 'Heriberto', 'Donnell', 'Cole', 'Denny', 'Davis', 'Gavin', 'Emery', 'Ward', 'Romeo', 'Jayson', 'Dion', 'Dante', 'Clement', 'Coy', 'Odell', 'Maxwell', 'Jarvis', 'Bruno', 'Issac', 'Mary', 'Dudley', 'Brock', 'Sanford', 'Colby', 'Carmelo', 'Barney', 'Nestor', 'Hollis', 'Stefan', 'Donny', 'Art', 'Linwood', 'Beau', 'Weldon', 'Galen', 'Isidro', 'Truman', 'Delmar', 'Johnathon', 'Silas', 'Frederic', 'Dick', 'Kirby', 'Irwin', 'Cruz', 'Merlin', 'Merrill', 'Charley', 'Marcelino', 'Lane', 'Harris', 'Cleo', 'Carlo', 'Trenton', 'Kurtis', 'Hunter', 'Aurelio', 'Winfred', 'Vito', 'Collin', 'Denver', 'Carter', 'Leonel', 'Emory', 'Pasquale', 'Mohammad', 'Mariano', 'Danial', 'Blair', 'Landon', 'Dirk', 'Branden', 'Adan', 'Numbers', 'Clair', 'Buford', 'German', 'Bernie', 'Wilmer', 'Joan', 'Emerson', 'Zachery', 'Fletcher', 'Jacques', 'Errol', 'Dalton', 'Monroe', 'Josue', 'Dominique', 'Edwardo', 'Booker', 'Wilford', 'Sonny', 'Shelton', 'Carson', 'Theron', 'Raymundo', 'Daren', 'Tristan', 'Houston', 'Robby', 'Lincoln', 'Jame', 'Genaro', 'Gale', 'Bennett', 'Octavio', 'Cornell', 'Laverne', 'Hung', 'Arron', 'Antony', 'Herschel', 'Alva', 'Giovanni', 'Garth', 'Cyrus', 'Cyril', 'Ronny', 'Stevie', 'Lon', 'Freeman', 'Erin', 'Duncan', 'Kennith', 'Carmine', 'Augustine', 'Young', 'Erich', 'Chadwick', 'Wilburn', 'Russ', 'Reid', 'Myles', 'Anderson', 'Morton', 'Jonas', 'Forest', 'Mitchel', 'Mervin', 'Zane', 'Rich', 'Jamel', 'Lazaro', 'Alphonse', 'Randell', 'Major', 'Johnie', 'Jarrett', 'Brooks', 'Ariel', 'Abdul', 'Dusty', 'Luciano', 'Lindsey', 'Tracey', 'Seymour', 'Scottie', 'Eugenio', 'Mohammed', 'Sandy', 'Valentin', 'Chance', 'Arnulfo', 'Lucien', 'Ferdinand', 'Thad', 'Ezra', 'Sydney', 'Aldo', 'Rubin', 'Royal', 'Mitch', 'Earle', 'Abe', 'Wyatt', 'Marquis', 'Lanny', 'Kareem', 'Jamar', 'Boris', 'Isiah', 'Emile', 'Elmo', 'Aron', 'Leopoldo', 'Everette', 'Josef', 'Gail', 'Eloy', 'Dorian', 'Rodrick', 'Reinaldo', 'Lucio', 'Jerrod', 'Weston', 'Hershel', 'Barton', 'Parker', 'Lemuel', 'Lavern', 'Burt', 'Jules', 'Gil', 'Eliseo', 'Ahmad', 'Nigel', 'Efren', 'Antwan', 'Alden', 'Margarito', 'Coleman', 'Refugio', 'Dino', 'Osvaldo', 'Les', 'Deandre', 'Normand', 'Kieth', 'Ivory', 'Andrea', 'Trey', 'Norberto', 'Napoleon', 'Jerold', 'Fritz', 'Rosendo', 'Milford', 'Sang', 'Deon', 'Christoper', 'Alfonzo', 'Lyman', 'Josiah', 'Brant', 'Wilton', 'Rico', 'Jamaal', 'Dewitt', 'Carol', 'Brenton', 'Yong', 'Olin', 'Foster', 'Faustino', 'Claudio', 'Judson', 'Gino', 'Edgardo', 'Berry', 'Alec', 'Tanner', 'Jarred', 'Donn', 'Trinidad', 'Tad', 'Shirley', 'Prince', 'Porfirio', 'Odis', 'Maria', 'Lenard', 'Chauncey', 'Chang', 'Tod', 'Mel', 'Marcelo', 'Kory', 'Augustus', 'Keven', 'Hilario', 'Bud', 'Sal', 'Rosario', 'Orval', 'Mauro', 'Dannie', 'Zachariah', 'Olen', 'Anibal', 'Milo', 'Jed', 'Frances', 'Thanh', 'Dillon', 'Amado', 'Newton', 'Connie', 'Lenny', 'Tory', 'Richie', 'Lupe', 'Horacio', 'Brice', 'Mohamed', 'Delmer', 'Dario', 'Reyes', 'Dee', 'Mac', 'Jonah', 'Jerrold', 'Robt', 'Hank', 'Sung', 'Rupert', 'Rolland', 'Kenton', 'Damion', 'Chi', 'Antone', 'Waldo', 'Fredric', 'Bradly', 'Quinn', 'Kip', 'Burl', 'Walker', 'Tyree', 'Jefferey', 'Ahmed', 'Willy', 'Stanford', 'Oren', 'Noble', 'Moshe', 'Mikel', 'Enoch', 'Brendon', 'Quintin', 'Jamison', 'Florencio', 'Darrick', 'Tobias', 'Minh', 'Hassan', 'Giuseppe', 'Demarcus', 'Cletus', 'Tyrell', 'Lyndon', 'Keenan', 'Werner', 'Theo', 'Geraldo', 'Lou', 'Columbus', 'Chet', 'Bertram', 'Markus', 'Huey', 'Hilton', 'Dwain', 'Donte', 'Tyron', 'Omer', 'Isaias', 'Hipolito', 'Fermin', 'Chung', 'Adalberto', 'Valentine', 'Jamey', 'Bo', 'Barrett', 'Whitney', 'Teodoro', 'Mckinley', 'Maximo', 'Garfield', 'Sol', 'Raleigh', 'Lawerence', 'Abram', 'Rashad', 'King', 'Emmitt', 'Daron', 'Chong', 'Samual', 'Paris', 'Otha', 'Miquel', 'Lacy', 'Eusebio', 'Dong', 'Domenic', 'Darron', 'Buster', 'Antonia', 'Wilber', 'Renato', 'Jc', 'Hoyt', 'Haywood', 'Ezekiel', 'Chas', 'Florentino', 'Elroy', 'Clemente', 'Arden', 'Neville', 'Kelley', 'Edison', 'Deshawn', 'Carrol', 'Shayne', 'Nathanial', 'Jordon', 'Danilo', 'Claud', 'Val', 'Sherwood', 'Raymon', 'Rayford', 'Cristobal', 'Ambrose', 'Titus', 'Hyman', 'Felton', 'Ezequiel', 'Erasmo', 'Stanton', 'Lonny', 'Len', 'Ike', 'Milan', 'Lino', 'Jarod', 'Herb', 'Andreas', 'Walton', 'Rhett', 'Palmer', 'Jude', 'Douglass', 'Cordell', 'Oswaldo', 'Ellsworth', 'Virgilio', 'Toney', 'Nathanael', 'Del', 'Britt', 'Benedict', 'Mose', 'Hong', 'Leigh', 'Johnson', 'Isreal', 'Gayle', 'Garret', 'Fausto', 'Asa', 'Arlen', 'Zack', 'Warner', 'Modesto', 'Francesco', 'Manual', 'Jae', 'Gaylord', 'Gaston', 'Filiberto', 'Deangelo', 'Michale', 'Granville', 'Wes', 'Malik', 'Zackary', 'Tuan', 'Nicky', 'Eldridge', 'Cristopher', 'Cortez', 'Antione', 'Malcom', 'Long', 'Korey', 'Jospeh', 'Colton', 'Waylon', 'Von', 'Hosea', 'Shad', 'Santo', 'Rudolf', 'Rolf', 'Rey', 'Renaldo', 'Marcellus', 'Lucius', 'Lesley', 'Kristofer', 'Boyce', 'Benton', 'Man', 'Kasey', 'Jewell', 'Hayden', 'Harland', 'Arnoldo', 'Rueben', 'Leandro', 'Kraig', 'Jerrell', 'Jeromy', 'Hobert', 'Cedrick', 'Arlie', 'Winford', 'Wally', 'Patricia', 'Luigi', 'Keneth', 'Jacinto', 'Graig', 'Franklyn', 'Edmundo', 'Sid', 'Porter', 'Leif', 'Lauren', 'Jeramy', 'Elisha', 'Buck', 'Willian', 'Vincenzo', 'Shon', 'Michal', 'Lynwood', 'Lindsay', 'Jewel', 'Jere', 'Hai', 'Elden', 'Dorsey', 'Darell', 'Broderick', 'Alonso'];
    var nameIndex = 0

    // These are the condition and counterbalancing ids

    // condition is which one is first
    this.condition = condition;
    // counterbalance is whether or not to show the two at the same time
    this.counterbalance = counterbalance;

    // Whether debug information should be printed out
    this.debug = true;
    // The amount of time to fade HTML elements in/out
    this.fade = 200;
    // List of trial information object for each experiment phase
    this.scenarios = new Object();

    // Canvas width and height
    this.canvas_width = 300;
    this.canvas_height = 300;

    // Lists of pages and examples for each instruction page.  We know
    // the list of pages we want to display a priori.
    this.instructions = {
        pages: ["instructions-training-1"]
    };

    // The list of all the HTML pages that need to be loaded
    this.pages = [
        "trial.html", 
        "submit.html"];

    // Parse the JSON object that we've requested and load it into the
    // configuration
    this.parse_config = function (data) {
        this.tests = data["tests"];
        this.scenarios = shuffle(data["scenarios"]); //shuffles the array 
        // this.scenarios = data["scenarios"]; 
        this.questions = data["questions"];
        this.choices = data["choices"];
        this.colors = data["colors"];
        this.prompt = data["prompt"];
        this.comments = data["comments"];
        this.makeNames();
    };

    // Load the experiment configuration from the server
    this.load_config = function () {
        var that = this;
        var jsonpath = "/static/json/n_exp3_stim.json";
        $.ajax({
            dataType: "json",
            url: jsonpath,
            async: false,
            success: function (data) { 
                if (that.debug) {
                    console.log("Got configuration data");
                }
                that.parse_config(data);
            }
        });
    };

    this.makeNames = function() { 
        for (var i = 0; i < this.scenarios.length; i++) {
            var sceneNames = {};
            for (var j = 0; j < this.scenarios[i].games.length; j++) {
                for (var k = 0; k < this.scenarios[i].games[j].team1.length; k++) {
                    if (!sceneNames.hasOwnProperty(this.scenarios[i].games[j].team1[k].toString())) {
                        var n = maleNames[nameIndex];
                        nameIndex++;
                        sceneNames[this.scenarios[i].games[j].team1[k].toString()] = n;
                        this.scenarios[i].games[j].team1[k] = n;
                    } else {
                        var num = this.scenarios[i].games[j].team1[k];
                        this.scenarios[i].games[j].team1[k] = sceneNames[num.toString()];
                    }
                }
                for (var k = 0; k < this.scenarios[i].games[j].team2.length; k++) {
                    if (!sceneNames.hasOwnProperty(this.scenarios[i].games[j].team2[k].toString())) {
                        var n = maleNames[nameIndex]
                        nameIndex++;
                        sceneNames[this.scenarios[i].games[j].team2[k].toString()] = n;
                        this.scenarios[i].games[j].team2[k] = n;
                    } else {
                        var num = this.scenarios[i].games[j].team2[k];
                        this.scenarios[i].games[j].team2[k] = sceneNames[num.toString()];
                    }
                }
            }
            console.log("pre comments: ", this.scenarios[i].comments);

            for (var j = 0; j < this.scenarios[i].wonngames.length; j++) {
                console.log("SCENE NAMES" ,sceneNames)
                console.log("scenarios comments: ", i, j, this.scenarios[i].wonngames[0], this.comments[0])
                var wonngames_info = this.scenarios[i].wonngames[j];
                console.log("datum before: ", wonngames_info.player, sceneNames);
                wonngames_info.player = sceneNames[wonngames_info.player]; //.[wonngames_info.player];
                console.log("datum: ", wonngames_info, sceneNames);
                this.scenarios[i].comments[j] = Mustache.render(this.comments[0], wonngames_info);
            }
            var offset = this.scenarios[i].comments.length;

            for (var j = 0; j < this.scenarios[i].closegames.length; j++) {
                var closegame_info = {"match" : this.scenarios[i].closegames[j]};
                this.scenarios[i].comments[offset + j] = Mustache.render(this.comments[1], closegame_info);
            }

            offset = this.scenarios[i].comments.length;

            for (var j = 0; j < this.scenarios[i].whoiswhat.length; j++) {
                var whoiswhat_info = this.scenarios[i].whoiswhat[j];
                this.scenarios[i].comments[offset + j] = Mustache.render(this.comments[2], whoiswhat_info);
            }

            for (var j = 0; j < this.scenarios[i].questions.length; j++) {
                if (this.scenarios[i].questions[j] == 0) {
                    this.scenarios[i].subjects[j].player = sceneNames[this.scenarios[i].subjects[j].player.toString()];
                } else if (this.scenarios[i].questions[j] == 1) {
                    this.scenarios[i].subjects[j].player = sceneNames[this.scenarios[i].subjects[j].player.toString()];
                } else if (this.scenarios[i].questions[j] == 2) {
                    this.scenarios[i].subjects[j].player1 = sceneNames[this.scenarios[i].subjects[j].player1.toString()];
                    this.scenarios[i].subjects[j].player2 = sceneNames[this.scenarios[i].subjects[j].player2.toString()];
                }
            }
        }
    }

    // Request from the server configuration information for this run
    // of the experiment
    this.load_config();
};
