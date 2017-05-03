import React, { PropTypes } from 'react';
import classnames from 'classnames';
import translate from '../../../constants/names';


const FilterView = ({ children, optionsLength, alias, onClick, activeClass }) => {
    const styleNames = classnames('list-group-item', 'button--hover', { active: activeClass });
    return (
        <div className="list-group">
            <button
                className={styleNames}
                onClick = {onClick}
            >
                <span
                    className="badge"
                >
                    {`${optionsLength || '?'}`}
                </span>
                {`${translate.filterHeaders[alias] || alias}`}
                <i className="fa fa-caret-down" />
            </button>
            <div className="second--sub--menu">
                { children }
            </div>
        </div>
    );
};


FilterView.propTypes = {
    activeClass  : PropTypes.bool,
    alias        : PropTypes.string.isRequired,
    children     : PropTypes.node,
    optionsLength: PropTypes.number.isRequired,
    onClick      : PropTypes.func.isRequired,
};

export default FilterView;
