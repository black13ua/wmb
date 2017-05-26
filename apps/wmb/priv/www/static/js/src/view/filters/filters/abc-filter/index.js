import React, { PropTypes } from 'react';
import { ListItem } from 'react-toolbox';
import translate from '../../../../constants/names';


const AbcFilterView = ({ onClick, optionsLength, alias, children }) =>
    <div onClick = {onClick}>
        <ListItem
            caption = {`${translate.filterHeaders[alias] || alias}`}
        />
        { children }
    </div>;


AbcFilterView.propTypes = {
    alias        : PropTypes.string.isRequired,
    children     : PropTypes.node,
    optionsLength: PropTypes.number.isRequired,
    onClick      : PropTypes.func.isRequired,
};

export default AbcFilterView;
