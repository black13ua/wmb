import React from 'react';

const FilterView = () =>
    <aside className="aside-left">
        <section className="random">
            <button data-id="5" className="add-random">+5 Random Tracks</button>
            <label htmlFor="autoload">
                <span>autoload</span>
                <input
                    type     = "checkbox"
                    id       = "autoload"
                    onChange = {() => console.log('autoLoadToggle()')}
                    checked  = ""
                />
            </label>
        </section>

        <section className="filters">
            <h3 className="filters-header">Filters:</h3>

            <div className="filter-genres">
                <label htmlFor="filter-genres">
                    <b>11</b><span>Genres:</span>
                    <select id="filter-genres">

                        <option value="Blues">Blues</option>

                        <option value="Folk">Folk</option>

                        <option value="Hard Bop">Hard Bop</option>

                        <option value="Heavy Metal">Heavy Metal</option>

                        <option value="Indie Rock">Indie Rock</option>

                        <option value="Jazz">Jazz</option>

                        <option value="New Age">New Age</option>

                        <option value="Pop">Pop</option>

                        <option value="Post Rock">Post Rock</option>

                        <option value="Progressive Rock">Progressive Rock</option>

                        <option value="Rock">Rock</option>

                    </select>
                </label>
            </div>

            <div className="filter-date">
                <label htmlFor="filter-date">
                    <b>30</b><span>Dates:</span>
                    <select id="filter-date">

                        <option value="1954">1954</option>

                        <option value="1955">1955</option>

                        <option value="1956">1956</option>

                        <option value="1958">1958</option>

                        <option value="1959">1959</option>

                        <option value="1961">1961</option>

                        <option value="1962">1962</option>

                        <option value="1963">1963</option>

                        <option value="1971">1971</option>

                        <option value="1973">1973</option>

                        <option value="1975">1975</option>

                        <option value="1976">1976</option>

                        <option value="1979">1979</option>

                        <option value="1986">1986</option>

                        <option value="1989">1989</option>

                        <option value="1992">1992</option>

                        <option value="1993">1993</option>

                        <option value="1996">1996</option>

                        <option value="1999">1999</option>

                        <option value="2000">2000</option>

                        <option value="2002">2002</option>

                        <option value="2004">2004</option>

                        <option value="2005">2005</option>

                        <option value="2006">2006</option>

                        <option value="2007">2007</option>

                        <option value="2010">2010</option>

                        <option value="2011">2011</option>

                        <option value="2012">2012</option>

                        <option value="2014">2014</option>

                        <option value="2015">2015</option>

                    </select>
                </label>
            </div>

            <h3 className="filters-header">Search:</h3>

            <div className="filter-name">
                <label htmlFor="textSearch">
                    <input type="text" id="textSearch" placeholder="type your favorit artist" />
                </label>
                <button>find it!</button>
            </div>

            <h3 className="filters-header">Alphabetical:</h3>

            <div className="filter-abc">
                <label htmlFor="filter-abc">
                    <b>9</b><span>Letters:</span>
                    <select name="filter-abc" id="filter-abc">
                        <option value="A">A</option>
                        <option value="B">B</option>
                        <option value="C">C</option>
                        <option value="D">D</option>
                        <option value="E">E</option>
                        <option value="F">F</option>
                        <option value="G">G</option>
                        <option value="H">H</option>
                        <option value="I">I</option>
                    </select>
                </label>
            </div>
        </section>
    </aside>;

export default FilterView;
